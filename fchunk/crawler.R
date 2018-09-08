
g_html = function()
{
  urls <- c()
  
  
  for(i in 1:5){
    url <- paste("https://www.allrecipes.com/?page=",toString(i),sep = "")
    webpage <- read_html(url)
    recipes_links_nodes <- html_nodes(webpage,'.fixed-recipe-card__title-link')
    recipes_links <- html_attr(recipes_links_nodes, "href")
    recipes_links <- recipes_links[!is.na(recipes_links)]
    urls <- c(urls,recipes_links)
  }
  
  
  dir.create("recipes")
  i = 1
  
  for(url in urls){
    webpage <- read_html(url)
    steps_html <- html_nodes(webpage,'.recipe-directions__list--item')
    steps_data <- html_text(steps_html)
    steps_data <- gsub("\n","",steps_data)
    steps_data <- gsub("            ","",steps_data)
    file_name <- paste("recipes/recipe",i,sep = "")
    file_name <- paste(file_name,".txt",sep = "")
    file <- file(file_name)
    writeLines(steps_data,file)
    close(file)
    i = i + 1
  }
}