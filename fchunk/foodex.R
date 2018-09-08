library(coreNLP)


Gtokenize = function(str)
{
  annotObj = annotateString(str)

  return(annotObj)

}
