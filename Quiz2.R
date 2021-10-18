capstone_quiz2 <- function() {
  
  library(stringr)
  
  file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  file_nm <- "dataset.zip"
  
  if(!file.exists(file_nm)) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","dataset.zip")
    unzip(file_nm)
  }
  
  con_twitter <- file("final/en_US/en_US.twitter.txt", "r")
  con_blogs <- file("final/en_US/en_US.blogs.txt", "r")
  con_news <- file("final/en_US/en_US.news.txt", "r")
  
  twitter <- readLines(con_twitter, -1)
  blogs <- readLines(con_blogs, -1)
  news <- readLines(con_news, -1)
  
  sentences<-scan("final/en_US/en_US.news.txt","character",sep="\n");
  #Replace full stop and comma
  sentences<-gsub("\\.","",sentences)
  sentences<-gsub("\\,","",sentences)
  #Split sentence
  words<-strsplit(sentences," ")
  #Calculate word frequencies
  words.freq<-table(unlist(words));
  freq <- data.frame(names(words.freq), as.integer(words.freq))
  colnames(freq) <- c("words","frequency")
  freq <- freq[order(freq$frequency,decreasing = T),]
  freq$cumfreq <- cumsum(freq$frequency)
  freq$percum <- freq$cumfreq/sum(freq$frequency)
  
  top50per <- freq[freq$percum<=.5,]
  top90per <- freq[freq$percum<=.9,]
  
  library(dplyr)
  library(tidytext)
  sen <- as.data.frame(sentences)
  bigrams <- unnest_tokens(sen,bigram,sentences,token = "ngrams", n=2)
  count(bigrams, bigram, sort = TRUE)
  
  
  
  
}