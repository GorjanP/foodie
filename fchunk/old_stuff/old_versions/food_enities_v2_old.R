# brackets confuse it for some reason
#change RBR and LBR to ( ) manually? and tag to PUNC

library(NLP)
library(coreNLP)
library(functional)
library(stringr)
library(tm)
library(gdata)
library(openNLP)
library(data.tree)
library(igraph)

#-----FUNCTIONS-----#

# function for spliting text on sentences

check_duplicate_recipes <- function()
{
  
  
  full_files1 = list.files("allrecipes1/recipes/", full.names = TRUE)
  files1 = list.files("allrecipes1/recipes/", full.names = FALSE)
  
  data1 = list()
  
  for(f in full_files1)
  {
    s <- readChar(f, file.info(f)$size)
    data1 <- append(data1, s)
  }
  
  
  full_files2 = list.files("allrecipes2/recipes/", full.names = TRUE)
  files2 = list.files("allrecipes2/recipes/", full.names = FALSE)
  
  data2 = list()
  
  for(f in full_files2)
  {
    s <- readChar(f, file.info(f)$size)
    data2 <- append(data2, s)
  }
  
  print(length(data1))
  print(length(data2))
  
  for(i in 1:(length(data1)-1))
  {
    if(i+1 > length(data2))
      break
    
    for(j in (i+1):length(data2))
    {
      if(data1[[i]] == data2[[j]])
      {
        print(paste(files1[[i]], " with ", files2[[j]]))
        #do.call(file.remove, list(full_files2[[j]]))
        
      }
    }
  }
}

tagger <- function(text)
{
  command <- 'java -cp .;./lib/UcrelSemTaggerClient.jar ApiTagger '
  text <- paste('"',text,sep = "")
  text <- paste(text,'"',sep = "")
  command <- paste(command,text,sep = "")
  result <- system(command,intern = TRUE)
  #result <- system('java -cp .;../lib/UcrelSemTaggerClient.jar ApiTagger \"Whisk chicken broth and brown sugar in a bowl\"',intern = TRUE)
  return(result)
}

parse_to_graph <- function(ptext)
{
  stopifnot(require(NLP) && require(igraph))
  
  ## Replace words with unique versions
  ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
  words <- regmatches(ptext, ms)[[1]]                                   # just words
  regmatches(ptext, ms) <- list(paste(words,  seq.int(length(words)), sep = "|"))  # add id to words (make it 1. WORD, 2. WORD...)
  
  ## Going to construct an edgelist and pass that to igraph
  ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
  edgelist <- matrix('', nrow=length(words)-1, ncol=2)
  
  ## Function to fill in edgelist in place
  edgemaker <- (function() {
    i <- 0                                       # row counter
    g <- function(node) {                        # the recursive function
      if (inherits(node, "Tree")) {            # only recurse subtrees
        #if ((val <- node$value) != 'TOP|1') { # skip 'TOP' node (added '1' above) # was TOP1
          val <- node$value
          for (child in node$children) {
            childval <- if(inherits(child, "Tree")) child$value else child
            i <<- i+1
            edgelist[i,1:2] <<- c(val, childval)
          }
        #}
        invisible(lapply(node$children, g))
      }
    }
  })()
  
  ## Create the edgelist from the parse tree
  edgemaker(Tree_parse(ptext))
  #tree <- graph_from_edgelist(edgelist)
  #tree <- FromDataFrameNetwork(as.data.frame(edgelist))
  
  return (edgelist)
  #return(tree)
}

format_edgelist <- function(edgelist)
{
  len = nrow(edgelist)
  ret = matrix(nrow = len, ncol = 4)
  first_name = vector(length = len)
  first_id = vector(length = len)
  second_name = vector(length = len)
  second_id = vector(length = len)
  
  for(i in 1:len)
  {
    tokens1 = strsplit(edgelist[i,1], split = "[|]")[[1]]
    first_name[i] = tokens1[1]
    first_id[i] = tokens1[2]
    
    tokens2 = strsplit(edgelist[i,2], split = "[|]")[[1]]
    second_name[i] = tokens2[1]
    second_id[i] = tokens2[2]
  }
  ret[, 1] = first_name
  ret[, 2] = first_id
  ret[, 3] = second_name
  ret[, 4] = second_id
  
  return(ret)
}


Food_checking <- function(s, tagged_data)
{
  
  foods_check<-rep(0,nrow(tagged_data))
  object_check<-rep(0,nrow(tagged_data))
  colors_check<-rep(0,nrow(tagged_data))
  is_disallowed<-rep(0,nrow(tagged_data))
  
  #maybe add just O1.. or not
  
  for(i in 1:nrow(tagged_data) )
  {
   
    if(!(startsWith(tagged_data$POS[i], "NN") || startsWith(tagged_data$POS[i], "JJ"))) 
    {
      next
    }
    
    if(length(grep("Z99", tagged_data$sem1[i], value = FALSE)) > 0)
    {
      #n
      t_z <- grep("AG.01.(d|e|f|g|h|i|j|k|l|m|y|z)|AF|AE", tagged_data$sem2[i], value = FALSE)      
    
      if(length(t_z) > 0)
        foods_check[i] <- 1
      
      next
    }
    
    
    # also check if it's a noun?
    # F - Food, L - Living thing, O1 - substance, chaneg to only O1.2?, try removing M1
    #t <- grep("F(1|2|3|4)|L(2|3)|O1\\.(1|2)", tagged_data$sem1[i], value = FALSE)
    t <- grep("F(1|2|3|4)|L(2|3)|O1.2", tagged_data$sem1[i], value = FALSE) 
    
    # B1 - Anatomy and physiology but not N4 - numbers or M6 - location
    t1 <- grep("B1", tagged_data$sem1[i], value = FALSE)
    t2 <- grep("N4|M6|O4.5", tagged_data$sem1[i], value = FALSE)
    
    # not O2 - objects or B5 - clothes and belongings, try removing M1 and O1.1, check how many are foods AND objects
    t3 <- grep("O2/F1|B5|N5|F1/O2", tagged_data$sem1[i], value = FALSE)
    t_eq <- grep("AG.01.t.08|AG.01.u|AH.02", tagged_data$sem2[i], value = FALSE)
    t_temp <- grep("O4.6|N3", tagged_data$sem1[i], value = FALSE)
    
    
    
    cond1 <- (length(t) > 0)
    cond2 <- ((length(t1) > 0) && (length(t2) == 0) )
    cond3 <- (length(t3) > 0) || length(t_eq > 0)
    
    if((cond1 || cond2) && !cond3)
    {
      foods_check[i] <- 1
    }
    
    # is O2, N, or M
    t4 <- grep("O2|B5", tagged_data$sem1[i], value = FALSE)
    t5 <- grep("L(2|3)", tagged_data$sem1[i], value = FALSE)
    cond4 <- (length(t4) > 0)
    cond5 <- (length(t1) > 0)
    cond6 <- (length(t5) > 0)
    
    if(cond4 && !cond5 && !cond6 && foods_check[i] == 0)
    {
      object_check[i] <- 1
    }
    
    
    t7 <- grep("O4.3", tagged_data$sem1[i], value = FALSE)
    cond7 <- (length(t7) > 0)
    
    if(cond7)
    {
      colors_check[i] <- 1
    }
    
    cond_eq <- (length(t_eq) > 0)
    cond_temp <- (length(t_temp) > 0)
    if(cond_eq || cond_temp)
      is_disallowed[i] <- 1
    
  
    }
  
  fc <<- foods_check
  ret = list(foods_check = foods_check, objects_check = object_check, colors_check = colors_check, disallowed_check = is_disallowed)
  return(ret)
}

write_to_file_text = function(ann, is_food, tag_data, is_object, is_color, is_disallowed, drva)
{
  #tokens
  cat(tag_data$token, sep="\n", file = "data/tokens.txt")
  
  #lemmas
  cat(tag_data$lemma, sep="\n", file = "data/lemmas.txt")
  
  # #gov_idx
  # cat(ann$basicDep$governorIdx, sep="\n", file = "data/govidx.txt")
  # 
  # #dep_idx
  # cat(ann$basicDep$dependentIdx, sep="\n", file = "data/depidx.txt")
  
  #POS
  cat(ann$token$POS, sep="\n", file = "data/POS.txt")
  
  #is_food
  cat(is_food, sep="\n", file = "data/is_food.txt")
  
  #is_object
  cat(is_object, sep="\n", file = "data/is_object.txt")
  
  #is_colors
  cat(is_color, sep="\n", file = "data/is_color.txt")
  
  #is_disallowed
  cat(is_disallowed, sep="\n", file = "data/is_disallowed.txt")
  
  
  #sentence
  cat(ann$token$sentence, sep="\n", file = "data/sentence_ids.txt")
  
  #roots
  cat(as.vector(ann$basicDep[ann$basicDep$governor == "ROOT", 3]), sep = "\n", file = "data/roots.txt")
  
  #root ids
  cat(as.vector(ann$basicDep[ann$basicDep$governor == "ROOT", 6]), sep = "\n", file = "data/root_ids.txt")
  
  len = length(drva)
  cat(len, sep = "\n", file = "data/trees/count.txt")
  
  for(i in 1:len)
  {
    maks = max(max(strtoi(drva[[i]][,2])), max(strtoi(drva[[i]][,4])))
    cat(maks, sep = "\n", file = "data/trees/count.txt", append = TRUE)
    
    cat(drva[[i]][,1], sep = "\n", file = paste("data/trees/", toString(i), "A.txt", sep = ""))
    cat(drva[[i]][,2], sep = "\n", file = paste("data/trees/",toString(i), "B.txt", sep = ""))
    cat(drva[[i]][,3], sep = "\n", file = paste("data/trees/",toString(i), "C.txt", sep = ""))
    cat(drva[[i]][,4], sep = "\n", file = paste("data/trees/",toString(i), "D.txt", sep = ""))
  }
}




fixIndeces_text = function(ann)
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
  
  
  ## !!!!!!!!!!!!!!! FIX THIS, CAN HAVE MULTIPLE JJ for a NN
  for(i in 1:(length(ann$token$POS)-1))
  {
    #check occurances of VBN + NNX
    if( (ann$token$POS[i] %in% c("VBD", "VBN") && ann$token$POS[i+1] %in% c("NN", "NNS", "NNP", "NNPS")))
    {
      ann$token$POS[i] = "JJ"
    }
  }
  
  return(ann)

}

#do something about brackets here
fixFractions <- function(s)
{
  ret <- s
  
  raw <- unlist(str_match_all(string = s, pattern = "[1-9] [1-9]/[1-9]"))
  if(length(raw) == 0)
  {
    return(ret)  
  }
  fractions <- c()
  wholes <- c()
  
  for(s in raw)
  {
    s1 <- str_split(s, pattern = " ")
    fractions <- append(fractions, s1[[1]][2])
    wholes <- append(wholes, as.numeric(s1[[1]][1]))
  }
  
  fractions <- as.vector(sapply(fractions, function(x) eval(parse(text=x))))
  #print(class(fractions))
  #print(class(wholes))
  
  
  for(i in 1:length(raw))
  {
    #print("------")
    #print(wholes[i])
    #print(fractions[i])
    num <- wholes[i] + fractions[i] 
    t_rep <- toString(num)
    ret = gsub(x = ret, pattern = raw[i], replacement = t_rep)
  }
  
  return(ret)
  
}

fixFractions_small <- function(s)
{
  ret <- s
  
  raw <- unlist(str_match_all(string = s, pattern = "[1-9]/[1-9]"))
  if(length(raw) == 0)
  {
    return(ret)  
  }
  fractions <- c()
  
  for(s in raw)
  {
    fractions <- append(fractions, s)
  }
  
  fractions <- as.vector(sapply(fractions, function(x) eval(parse(text=x))))
  #print(class(fractions))
  #print(class(wholes))
  
  
  for(i in 1:length(raw))
  {
    #print("------")
    #print(wholes[i])
    #print(fractions[i])
    num <- fractions[i] 
    t_rep <- toString(num)
    ret = gsub(x = ret, pattern = raw[i], replacement = t_rep)
  }
  
  return(ret)
  
}

to_reduced_POS <- function(tag_data, ann)
{
  ret = rep(-1, nrow(tag_data))
  
  for(i in 1:nrow(tag_data))
  {
    prv = tag_data$POS[i]
    vtor = ann$token$POS[i]
    
    
    # rules
    
    if(prv == vtor)
    {
      ret[i] <- vtor
    }
    else if(startsWith(prv, "V") && startsWith(vtor, "V"))
    {
      ret[i] <- vtor
    }
    else if(startsWith(prv, "N") && startsWith(vtor, "N"))
    {
      ret[i] <- vtor
    }
    else if(startsWith(prv, "J") && startsWith(vtor, "J"))
    {
      ret[i] <- vtor
    }
    else if(startsWith(prv, "V") || startsWith(vtor, "V"))
    {
      # to catch the false positives (e.g. where 'heat' should be a verb, but it's classified as a noun)
      # low probability of unambiguous verb misclassification (e.g "has")
      
      if(startsWith(vtor, "V"))
      {
        ret[i] <- vtor
      }
      else if(prv %in% c("VABI","VABM","VVB0","VVBI","VVBM", "VAD0","VVD0","VVDI","VAH0","VAHI", "VVH0", "VVHI", "VM", "VMK", "VV0", "VVI"))
      {
        ret[i] <- "VB"
      }
      else if(prv %in% c("VABDR","VABDZ","VVBDR","VVBDZ","VADD","VVDD","VAHD","VVHD","VVD"))
      {
        ret[i] <- "VBD"
      }
      else if(prv %in% c("VABN","VVBN","VVDN","VVHN","VVN", "VVNK"))
      {
        ret[i] <- "VBN"
      }
      else if(prv %in% c("VABG","VVBG","VVDG","VAHG","VVHG","VVG", "VVGK"))
      {
        ret[i] <- "VBG"
      }
      else if(prv %in% c("VABR", "VABZ", "VVBR", "VVBZ", "VADZ", "VAHZ", "VVHZ", "VVZ"))
      {
        ret[i] <- "VBZ"
      }
      else 
      {
        
        print("!!!!WTF!!!!")
        print(prv)
        print(tag_data$token[i])
      }
    }
    else
    {
      ret[i] <- vtor
    }
    
    # static mapping?
  }
  
  
  return(ret)
}

#ensemble from USAS API and coreNLP, using USAS API code scheme
to_extended_POS <- function(tag_data, ann)
{
  ret = rep(-1, nrow(tag_data))
  
  # print("--------")
  # print(nrow(tag_data))
  # print(nrow(ann$token))
  # print(tag_data$token)
  # print(ann$token$token)
  
  for(i in 1:nrow(tag_data))
  {
    prv = tag_data$POS[i]
    vtor = ann$token$POS[i]
    prv_word = tag_data$token[i]
    vtor_word = tag_data$token[i]
    
    
    
    # rules (try to catch "hardcoded" verbs such as 'are' , 'be', etc.)
    if(prv == vtor)
    {
      ret[i] <- prv
    }
    else if(startsWith(prv, "V") && startsWith(vtor, "V"))
    {
      ret[i] <- prv
    }
    else if(startsWith(prv, "N") && startsWith(vtor, "N"))
    {
      ret[i] <- prv
    }
    else if(startsWith(prv, "J") && startsWith(vtor, "J"))
    {
      ret[i] <- prv
    }
    else if(startsWith(prv, "V") && startsWith(vtor, "J"))
    {
      ret[i] <- vtor
    }
    else if(startsWith(prv, "V") || startsWith(vtor, "V"))
    {
      # to catch the false positives (e.g. where 'heat' should be a verb, but it's classified as a noun)
      # low probability of unambiguous verb misclassification (e.g "has")s
      
      if(startsWith(prv, "V"))
      {
        ret[i] <- prv
      }
      else if(vtor == "VB")
      {
        ret[i] <- "VV0"
      }
      else if(vtor == "VBD")
      {
        ret[i] <- "VVD"
      }
      else if(vtor == "VBG")
      {
        ret[i] <- "VVG"
      }
      else if(vtor == "VBN")
      {
        ret[i] <- "VVN"
      }
      else if(vtor == "VBZ")
      {
        ret[i] <- "VVZ"
      }
      else if(ret[i] == "VBP")
      {
        ret[i] <- "VV0"
      }
      else
      {
        print("--------")
        print(paste(prv, prv_word,sep = " "))
        print(paste(vtor, vtor_word,sep = " "))
        ret[i] <- prv
      }
    }
    else
    {
      ret[i] <- prv
    }
    
    # static mapping?
  }
  
  
  return(ret)
}

#ADD THE FIX FOR VBN + NN TO JJ + NN !!!
format_tags <- function(raw_tags)
{
  ret = data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
  colnames(ret) = c("token", "lemma", "POS", "sem1", "MWE", "sem2")
  
  for(term in raw_tags)
  {
    s = strsplit(term, '\t')[[1]]
    if(s[1] %in% c("S_BEGIN", "S_END") || length(s) == 0)
    {
      next
    }
    s = list(token = s[1], lemma = s[2], POS = s[3], sem1 = s[4], MWE = s[5], sem2 = s[6])
    
    ret = rbind(ret, data.frame(s, stringsAsFactors = FALSE))
    
  }
  
  ret = data.frame(ret, stringsAsFactors = FALSE)
  return(ret)
}

parse_recipe <- function(num, close = TRUE)
{
  if(missing(num))
  {
    num = 1
  }
  
  invisible(do.call(file.remove, list(list.files("food_chunks", full.names = TRUE))))
  invisible(do.call(file.remove, list(list.files("graphs", full.names = TRUE))))
  invisible(do.call(file.remove, list(list.files("entities", full.names = TRUE))))
  invisible(do.call(file.remove, list(list.files("food_modifiers", full.names = TRUE))))
  
  full_files = list.files("recipes/", full.names = TRUE)
  files = list.files("recipes/", full.names = FALSE)
  
  sent_token_annotator <<- Maxent_Sent_Token_Annotator()
  word_token_annotator <<- Maxent_Word_Token_Annotator()
  parse_annotator <<- Parse_Annotator()
  pos_tag_annotator <<- Maxent_POS_Tag_Annotator()
  
  ctr = 1
  ret = list()
  ret2 = list()
  grafoj = list()
  for(f in full_files)
  {
    
    if(ctr > num)
      break;
    
    invisible(do.call(file.remove, list(list.files("data", full.names = TRUE))))
    invisible(do.call(file.remove, list(list.files("data/trees", full.names = TRUE))))
    
    
    #test regexp
    s <<- readChar(f, file.info(f)$size)
    s <<- gsub("[\r\n\t\f\v ][\r\n\t\f\v ]+", " ", s)
    s <<- gsub(x = s, pattern = "\"", replacement = "")
    s <<- gsub(x = s, pattern = "°", replacement = " degrees ")
    
    s <<- fixFractions_small(fixFractions(s))
    
    
    #try adding openNLP POS tags to the ensemble, probably useless
    
    tag_data <- format_tags(tagger(s)) 
    #add more of these VXX to VVXX
    tag_data$POS[tag_data$POS == "VBR"] = "VVBR"
    
    ann <- fixIndeces_text(annotateString(s))
    ann_reduced <- ann
    
    t_extended <- to_extended_POS(tag_data = tag_data, ann = ann)
    t_reduced <- to_reduced_POS(tag_data = tag_data, ann = ann)
    
    tag_data$POS <- t_extended
    ann$token$POS <- t_extended
    ann_reduced$token$POS <- t_reduced
    
    cp_tag_data <<- tag_data
    cp_ann <<- ann
    cp_ann_reduced <<- ann_reduced
    
    t_food_object <- Food_checking(s, tag_data)
    food<- as.vector(t_food_object$foods_check)
    object<- as.vector(t_food_object$objects_check)
    color <- as.vector(t_food_object$colors_check)
    disallowed <- as.vector(t_food_object$disallowed_check)

    #print(ann$token$POS)
    #print(tag_data$POS)
    
    
    t_grafoj = list()
    for(i in 1:length(ann_reduced$parse))
    {
      ann_reduced$parse[i] <- gsub(x = ann_reduced$parse[i], pattern = "[\r\n\t\f\v ][\r\n\t\f\v ]+", " ")
      #ann_reduced$parse[i] <- gsub(x = ann_reduced$parse[i], pattern = "[\r\n]", "")
      ann_reduced$parse[i] = gsub(x = ann_reduced$parse[i], pattern = "ROOT", "TOP")
      t_grafoj[[i]] <- format_edgelist(parse_to_graph(ann_reduced$parse[i]))
    }
    ret[[ctr]] = ann_reduced
    grafoj[[ctr]] <- t_grafoj
    #ret2[[ctr]] = trees
    
    
    write_to_file_text(ann, food, tag_data, object, color, disallowed, grafoj[[ctr]])
    
    command = paste("FC_v3.exe", files[ctr], sep = " ")
    print(paste("Executing command: ", command))
    system(command)
    
    ctr = ctr + 1
    
    
    
  }
  
  #OLD GRAPH CODE GOES HERE
  
  #Cleanup
  if(close)
  {
    
    #driver$client$close()
    rm(list = setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv) ), envir = .GlobalEnv)
    rm(list = c("parse_annotator", "pos_tag_annotator", "sent_token_annotator", "word_token_annotator"), envir = .GlobalEnv)
    invisible(gc())
   
  }
  return(list(ann = ret, graphs = grafoj))
  #return(list(core=ret, open = ret2))
}
  

  #----Curried----#
  #for graph node visualization
  Gstrwidth = Curry(strwidth, unit = "inches")
  Gstrheight = Curry(strheight, unit = "inches")
  #----End Curried----#

#-----END-FUNCTIONS-----#
 
