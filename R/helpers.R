#' @title  Download and parse webpage
#' @author Gareth Burns
#' @rdname html_to_text
#' @name html_to_text
#' @description Function to create a extraction text from website
#' @param url url of website
#' @return character vestor of text from website
#' @import 'RCurl' 'XML' 
#' @references \url{http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need} 
#' @export


html_to_text <- function(url){
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don’t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}