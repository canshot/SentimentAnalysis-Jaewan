library("rmongodb")
library("dplyr")
library("randomForest")
library("plyr")
library("class")
library("e1071")
library("nnet")
library("neuralnet")
library("ranger")

book1texts <- read.csv(file="C:/Users/jae12/Box Sync/3. IS 804/CourseProject/Data/Mydf1_texts.csv", header=TRUE, sep=",")

#1.TOKENIZATION
tokenize = function(book1texts){

  # Lowercase all words for convenience
  doc <- tolower(book1texts)
  
  # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
  doc <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", doc)
  
  # Remove all punctuation
  doc <- gsub("[[:punct:]]", "", doc)
  
  # Remove all newline characters
  doc <- gsub("[\r\n]", "", doc)
  
  # Regex pattern for removing stop words
  stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse="|"), ")\\b")
  doc <- gsub(stop_pattern, "", doc)
  
  # Replace whitespace longer than 1 space with a single space
  doc <- gsub(" {2,}", " ", doc)
  
  # Split on spaces and return list of character vectors
  doc_words <- strsplit(doc, " ")
  return(doc_words)
}







  
