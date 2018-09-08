fixIndeces_ing = function(ann)
{
  t_idx = 1
  count = 0
  t_sentance = as.vector((table(ann$basicDep$sentence)))
  ann$basicDep$governorIdx = strtoi(ann$basicDep$governorIdx)
  ann$basicDep$dependentIdx = strtoi(ann$basicDep$dependentIdx)
  
  
  for(i in 1:max(ann$basicDep$sentence))
  {
    while(ann$basicDep$sentence[t_idx] == i)
    {
      
      ann$basicDep$governorIdx[t_idx] = ann$basicDep$governorIdx[t_idx] + count
      ann$basicDep$dependentIdx[t_idx] = ann$basicDep$dependentIdx[t_idx] + count
      t_idx = t_idx + 1
      if(t_idx > length(ann$basicDep$sentence))
        break
    }
    
    count = count + t_sentance[i]
  }
  
  for(i in 1:(length(ann$token$POS)-1))
  {
    #statically convert VDB and VBN ('ed' suffix verbs to JJ (adjectives))
    if( (ann$token$POS[i] %in% c("VBD", "VBN")) )
    {
      ann$token$POS[i] = "JJ"
    }
    
  }
  
  for(i in 1:(length(ann$token$POS)))
  {
    if(endsWith(ann$token$token[i], "less"))  
    {
      ann$token$POS[i] = "JJ"
    }
  }
  
  
  return(ann)
  
}

finalizeIng <- function(ing_food, ing_ann)
{
  ret = new.env()
  
  for(i in 1:max(ing_ann$token$sentence))
  {
    w_is_food = ing_food[ing_ann$token$sentence == i]
    w_POS = ing_ann$token$POS[ing_ann$token$sentence == i]
    w_words = ing_ann$token$token[ing_ann$token$sentence == i]
    adjectives = as.vector(w_words[w_POS == "JJ"])
    adjectives = unique(adjectives)

    ctr = 0
    str = ""
    for(i in 1:length(w_is_food))
    {
      if(w_is_food[i] == 1 && w_POS[i] %in% c("NN", "NNS", "NNP", "NPPS"))
      {
        ctr = ctr + 1
        str = paste(str, w_words[i], sep = " ")
      }
      else 
      {
        str = trim(str)
    
        if(ctr > 0)
        {
          if(!exists(str, envir = ret))
          {
            str = trim(str)
            assign(str, list(name=str, adj = adjectives), envir = ret)
          }
          else
          {
            
            #print("------------------------")
            #print(adjectives)
            #print(str)
            temp_ing = get(str, envir = ret)$adj
            temp_ing = c(temp_ing, adjectives)
            rm(str, envir = ret)
            assign(str, list(name = str, adj = temp_ing), envir = ret)
            #print("----------------------Duplicate ingredients!?------------------------")
          }
        }

        ctr = 0
        str = ""
      }
    }
    
    if(ctr > 0)
    {
      if(!exists(str, envir = ret))
      {
        
        assign(str, list(name=str, adj = adjectives), envir = ret)
      }
      else
      {
        temp_ing = get(str, envir = ret)$adj
        temp_ing = unique(c(temp_ing, adjectives))
        rm(str, envir = ret)
        assign(str, list(name = str, adj = temp_ing), envir = ret)
        print("----------------------Duplicate ingredients!?------------------------")
      }
    }
  }
    
  return(ret)
}


parse_ingr <- function(num, close = TRUE)
{
  if(missing(num))
  {
    num = 1
  }
  
  invisible(do.call(file.remove, list(list.files("ingr", full.names = TRUE))))
  
  ing_full_files = list.files("allrecipes/ingredients/", full.names = TRUE)
  ing_files = list.files("allrecipes/ingredients/", full.names = FALSE)
  
  
  #driver <<- rsDriver()
  #remDr <<- driver[["client"]]
  
  
 
  
  ctr = 1
  
  for(f in ing_full_files)
  {
    
    if(ctr > num)
      break;
    
    
    sent_token_annotator <<- Maxent_Sent_Token_Annotator() 
    word_token_annotator <<- Maxent_Word_Token_Annotator() 
    parse_annotator <<- Parse_Annotator() 
    pos_tag_annotator <<- Maxent_POS_Tag_Annotator()
    
    
    
    ing_con = file(ing_full_files[ctr], open = "r")
    
    allLines = ""
    while (length(oneLine <- readLines(ing_con, n = 1, warn = FALSE)) > 0)
    {
      oneLine = trim(oneLine)
      oneLine = paste(oneLine, ".", sep = "")
      allLines = paste(allLines, oneLine)
      
    }
    
    ing_ann <- fixIndeces_ing(annotateString(allLines))
    ing_food<-Food_checking(allLines,ing_ann)
    
    
    ing_map = finalizeIng(ing_food, ing_ann)
    print_env(ing_map, ing_files[ctr])
    
    
    
    ctr = ctr + 1
  }
  
  #Cleanup
  if(close)
  {
    
    #driver$client$close()
    rm(list = setdiff(ls(), lsf.str()))
  }
  
}



print_env <- function(env, file)
{
  path = paste("ingr/", file)
  for(str in ls(env))
  {
    obj = get(str, envir = env)
    cat(obj$name, file = path, append = TRUE)
    cat("\n", file = path, append = TRUE)
    cat(obj$adj, file = path, sep = ", ", append = TRUE)
    cat("\n", file = path, append = TRUE)
    cat("-------\n", file = path, append = TRUE)
    
  }
}





Default_chunking<-function(s, ann)
{
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(s, pos_tag_annotator, a2)
  features = as.vector(a3$features)
  
  ctr = 1
  for(i in 1:length(a3))
  {
    if(a3[i]$type == "word")
    {
      features[i][[1]][[1]] = ann$toke$POS[ctr]
      ctr = ctr + 1
    }
  }
  
  a3 <- Annotation(id = as.vector(a3$id), type = as.vector(a3$type), start = as.vector(a3$start), end = as.vector(a3$end), features = features)
    
  a3w <- subset(a3, type == "word") 
  tags <- sapply(a3w$features, `[[`, "POS")
  
  a4<-annotate(s, Maxent_Chunk_Annotator(), a3)
  a4w <- subset(a4, type == "word") 
  
  chunks<-sapply(a4w$features, `[[`, "chunk_tag")
  
  beginnings1<- grep("^B",chunks)
  beginnings2<-grep("^O",chunks)
  beginnings<-sort(c(beginnings1,beginnings2))
  
  
  return(list(chunks=chunks,beginnings=beginnings))
  
}


Chunk_matrix<-function(df_default_chunking,words)
{
  
  
  chunks<-df_default_chunking$chunks
  beginnings<-df_default_chunking$beginnings
  
  A<-matrix(0,length(words),length(beginnings))
  
  for(j in 1:(length(beginnings)-1)){
    
    for(i in 1:length(words)){
      
      if(i<beginnings[j+1] && i>=beginnings[j]){
        A[i,j]<-1
      }
      
      
    }
  }
  
  for( i in beginnings[length(beginnings)]:length(chunks)){
    A[i,length(beginnings)]<-1
  }
  
  
  return(A)
  
}

Candidates_matrix<-function(A,C)
{
  
  B<-t(A)%*%C
  B[B>=1]<-1
  
  return(B)
}



convert_text_to_sentences <- function(text, lang = "en")
{
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}


make_tree<-function(s, ann)
{
  
  annotated <- annotate(s, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
  features = as.vector(annotated$features)
  
  ctr = 1
  for(i in 1:length(annotated))
  {
    if(annotated[i]$type == "word")
    {
      features[i][[1]][[1]] = ann$token$POS[ctr]
      ctr = ctr + 1
    }
  }
  
  annotated <- Annotation(id = as.vector(annotated$id), type = as.vector(annotated$type), start = as.vector(annotated$start), end = as.vector(annotated$end), features = features)
  
  parsed <- parse_annotator(s, annotated)
  parsedtexts <- sapply(parsed$features, '[[', "parse")
  parsetrees <- lapply(parsedtexts, Tree_parse)
  
  return(list(parsedtexts, parsetrees)) 
  
}


    # GENERATE GRAPH DIFFERENTLY
    mtr = read.table(paste("graphs/", files[ctr], sep =""), sep=" ", header = FALSE)
    mtr = mtr[, -length(mtr)]
    mtr = as.matrix(mtr)
    labels = as.data.frame(read.table(paste("entities/", files[ctr], sep =""), sep="\n", header = FALSE))
    labels2 = gsub(x = as.vector(labels[[1]]), pattern = " ", replacement = "\n")


    graf = graph_from_adjacency_matrix(mtr, mode = "undirected", diag = FALSE)
    V(graf)$color = "white"
    V(graf)$label = as.vector(labels2)
    V(graf)$name = as.vector(labels2)
    #V(graf)$label = as.vector(labels[[1]])
    #V(graf)$name = as.vector(labels[[1]])

    #print(labels2)
    to_delete = c()
    for(i in 1:length(V(graf)))
    {
    #  print(i)
      #if(!(ann$token$POS[i] %in% c("JJ", "JJR", "JJS", "NN", "NNS", "NNP", "NNPS", "VB", "VBG", "VBN", "VBD", "VBZ", "VBP")))
      #{
      #  to_delete = append(to_delete, i)
      #}


      if(trim(labels2[i]) %in% c(".", ",", ";", ":", "-", "and", "with")) #other useless adverbs and prepositions
      {
        to_delete = append(to_delete, i)

        #labels2 = labels2[-i]
        #graf = delete_vertices(graf, i)
      }

    }

    #print(to_delete)
    labels2 = labels2[-to_delete]
    graf = delete_vertices(graf, to_delete)

    ret[[ctr]] = graf

    # foreach, count pixels, strwidth and height gives in inches
    #widths = apply(X = as.vector(labels), FUN = Gstrwidth, MARGIN = 1)
    #heights = apply(X = as.vector(labels), FUN = Gstrheight, MARGIN = 1)


    #V(graf)$label = 1:length(V(graf))
    #tkplot(graf)
    # END

