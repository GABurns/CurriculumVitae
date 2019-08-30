#' @title  Create Term-Document Matrix
#' @author Gareth Burns
#' @rdname createTermDocumentMatrix
#' @name createTermDocumentMatrix
#' @description Function to create a term-document matrix based on a text file/object.
#' @param x: character string (plain text, web url, txt file path)
#' @param type: specify whether x is a plain text, a web page url or a file path
#' @param lang: the language of the text
#' @param excludeWords: a vector of words to exclude from the text
#' @param textStemming: reduces words to their root form
#' @return a list(tdm, freqTable)
#' @import 'tm' 'SnowballC' 
#' @references \url{http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need} 
#' @export

createTermDocumentMatrix <- function(x, type = c("text", "url", "file"), 
                             lang = "english", excludeWords = NULL, 
                             textStemming = FALSE)
{ 

  type <- match.arg(type, choices =  c("text", "url", "file"), several.ok = FALSE)

  switch (type,
          "file" = text <- readLines(x),
          "url" = text <- html_to_text(x),
          "text" = text <- x
  )

  # Load the text as a corpus
  docs <- tm::VCorpus(tm::VectorSource(text))
  
  # Convert the text to lower case
  docs <- tm::tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  docs <- tm::tm_map(docs, removeNumbers)
  
  # Remove stopwords for the language 
  docs <- tm::tm_map(docs, removeWords, stopwords(lang))
  
  # Remove punctuations
  docs <- tm::tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm::tm_map(docs, removeWords, excludeWords) 
  
  # Text stemming
  if(textStemming) docs <- tm::tm_map(docs, stemDocument)
  
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  return(list(tdm=tdm, freqTable = d))
}


  

#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
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
