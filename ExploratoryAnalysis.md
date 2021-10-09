---
title: "ExploratoryAnalysis"
author: "Puneet Singla"
date: "10/9/2021"
output: 
  html_document:
    toc: true
    toc_depth: 3
    keep_md: true
    
---




```r
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
  library(wordcloud)
  library(ngram)
})
```

```
## Warning: package 'tidytext' was built under R version 4.1.1
```

```
## Warning: package 'tidyverse' was built under R version 4.1.1
```

```
## Warning: package 'readr' was built under R version 4.1.1
```

```
## Warning: package 'forcats' was built under R version 4.1.1
```

```
## Warning: package 'wordcloud' was built under R version 4.1.1
```

```
## Warning: package 'ngram' was built under R version 4.1.1
```

To hide the code complexity, i have written a R script to calculate the required stats and store the outputs in RDS files. If you are interested in viewing in the R script, [click here]( https://github.com/PuneetSingla5822/Capstone/ExploratoryAnalysis.R)


```r
repo_summary <- readRDS("clean_repos/repo_summary.rds")
tidy_repo <- readRDS("clean_repos/tidy_repo.rds")
cover_90  <- readRDS("clean_repos/cover_90.rds")
bigram_cover_90   <- readRDS("clean_repos/bigram_cover_90.rds")
trigram_cover_90  <- readRDS("clean_repos/trigram_cover_90.rds")
```

## Introduction   
This project analyzes the [HC Corpora Dataset](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) with the goal of creating a prediction model for predicting n-grams.  In this report, I will summarize the exploratory data analysis that i have conducted on the data.

## File Summary   
Data files provided are: blogs, news, and twitter. Here are few basic stats on the data files.


|f_names |   f_size| f_lines|    n_char|  n_words| pct_n_char| pct_lines| pct_words|
|:-------|--------:|-------:|---------:|--------:|----------:|---------:|---------:|
|blogs   | 200.4242|  899288| 208361438| 37334131|       0.54|      0.27|      0.53|
|news    | 196.2775|   77259|  15683765|  2643969|       0.04|      0.02|      0.04|
|twitter | 159.3641| 2360148| 162385035| 30373583|       0.42|      0.71|      0.43|
  
To speed up the processing, I have sampled 10% of the lines from the each file. Each data sample was cleaned and broken into uni, bi and tri-grams. To further speed up the model, i have subsetted the n-grams to cover 90% of the sample phrases.

## Uni-grams, word cloud  
Next, we will create a word cloud to see the most frequent words in the data.


```r
#' Word cloud
cover_90 %>%
  with(wordcloud(word, n, max.words = 100,
                 colors = brewer.pal(8, 'Dark2'), random.order = FALSE))
```

![](ExploratoryAnalysis_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Uni-grms, By Source  
Now, Let's look at the word frequencies in the data.


```r
#' Word distribution by source
freq <- tidy_repo %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n / sum(n)) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `blogs`:`twitter`) %>%
  arrange(desc(proportion), desc(n))
freq %>%
  filter(proportion > 0.002) %>% 
  mutate(word = reorder(word, proportion)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col(fill="blue") + 
  xlab(NULL) + 
  coord_flip() +
  theme_light() +
  facet_grid(~source, scales = "free")
```

![](ExploratoryAnalysis_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Uni-gram Distribution
Distributions were created for each set of n-grams, based on relative frequency.


```r
#' Word distribution
cover_90 %>%
  top_n(20, proportion) %>%
  mutate(word = reorder(word, proportion)) %>%
  ggplot(aes(word, proportion)) +
  geom_col(fill="blue") +
  xlab(NULL) +
  theme_light() +
  coord_flip()
```

![](ExploratoryAnalysis_files/figure-html/unigrams-1.png)<!-- -->

## Bi-gram Distribution

```r
#' Bigram distribution
bigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(bigram = reorder(bigram, proportion)) %>%
  ggplot(aes(bigram, proportion)) +
  geom_col(fill="blue") +
  xlab(NULL) +
  theme_light() +
  coord_flip()
```

![](ExploratoryAnalysis_files/figure-html/bigrams-1.png)<!-- -->

## Tri-gram Distribution

```r
#' trigram distribution
trigram_cover_90 %>%
  top_n(20, proportion) %>%
  mutate(trigram = reorder(trigram, proportion)) %>%
  ggplot(aes(trigram, proportion)) +
  geom_col(fill="blue") +
  xlab(NULL) +
  theme_light() +
  coord_flip()
```

![](ExploratoryAnalysis_files/figure-html/trigrams-1.png)<!-- -->

## N-gram Prediction Model

For the N-gram prediction model, I am going to use the n-gram tables created for bi-grams and tri-grams as the basis for prediction.  The user will input a word, the model will find the bi-gram with the greatest relative frequency given that word.  Similarly, the tri-gram table will be used for making predictions from two word entries and so on.  


```r
trigrams_separated <- trigram_cover_90 %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
knitr::kable(head(trigrams_separated))
```



|word1  |word2 |word3 |     n| proportion|  coverage|
|:------|:-----|:-----|-----:|----------:|---------:|
|NA     |NA    |NA    | 19499|  0.0035005| 0.0035005|
|thanks |for   |the   |  2449|  0.0004396| 0.0039401|
|one    |of    |the   |  2115|  0.0003797| 0.0043198|
|a      |lot   |of    |  1967|  0.0003531| 0.0046729|
|i      |want  |to    |  1365|  0.0002450| 0.0049180|
|to     |be    |a     |  1311|  0.0002354| 0.0051533|

In the above table, that the tri-grams are separated by word and arranged by relative frequency. When the user inputs two words, the model matches those words and then finds the third word with the greatest relative frequency.  Cases where there is no match, or where more than two words are entered, will have random completion.

