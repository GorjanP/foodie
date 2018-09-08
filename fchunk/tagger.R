tagger <- function(text){
  command <- 'java -cp .;./lib/UcrelSemTaggerClient.jar ApiTagger '
  text <- paste('"',text,sep = "")
  text <- paste(text,'"',sep = "")
  command <- paste(command,text,sep = "")
  result <- system(command,intern = TRUE)
  #result <- system('java -cp .;../lib/UcrelSemTaggerClient.jar ApiTagger \"Whisk chicken broth and brown sugar in a bowl\"',intern = TRUE)
  return(result)
}