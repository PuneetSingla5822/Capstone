capstone_quiz1 <- function() {
  
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
  
  print("Question1")
  print(file.size("final/en_US/en_US.blogs.txt")/(1024*1024))
  
  print("Question2")
  print(length(twitter)/(1000*1000))
  
  print("Question3")
  print(max(str_length(blogs)))
  print(max(str_length(news)))
  
  print("Question4")
  print(length(grep("love",twitter))/length(grep("hate",twitter)))
  
  print("Question5")
  print(twitter[grep("biostats",twitter)])
  
  print("Question6")
  print(length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",twitter)))
  
}