---
title: "MilestoneReport1"
author: "Puneet Singla"
date: "9/24/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r}
library(stringr)

## Download the files
file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
file_nm <- "dataset.zip"
  
if(!file.exists(file_nm)) {
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip","dataset.zip")
  unzip(file_nm)
}
```

```{r}
## Read the files
con_twitter <- file("final/en_US/en_US.twitter.txt", "r")
con_blogs <- file("final/en_US/en_US.blogs.txt", "r")
con_news <- file("final/en_US/en_US.news.txt", "r")
  
twitter <- readLines(con_twitter, -1)
blogs <- readLines(con_blogs, -1)
news <- readLines(con_news, -1)
```

