### Data Science Capstone : Final Project
### server.R file for the Shiny app
### It uses natural language processing algorithm to predict the next word in the sentence.

suppressPackageStartupMessages({
    library(tidyverse)
    library(stringr)
}
)
suppressWarnings(library(shiny))

## Load Training Data

bi_words <- readRDS("bi_words.rds")
tri_words  <- readRDS("tri_words.rds")
quad_words <- readRDS("quad_words.rds")
quint_words <- readRDS("quint_words.rds")
sext_words <- readRDS("sext_words.rds")

## Create Ngram Matching Functions

bigram <- function(input_words){
    num <- length(input_words)
    filter(bi_words,
           word1==input_words[num]) %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
    num <- length(input_words)
    filter(tri_words,
           word1==input_words[num-1],
           word2==input_words[num])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
    num <- length(input_words)
    filter(quad_words,
           word1==input_words[num-2],
           word2==input_words[num-1],
           word3==input_words[num])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    ifelse(out=="character(0)", trigram(input_words), return(out))
}

quintgram <- function(input_words){
    num <- length(input_words)
    filter(quint_words,
           word1==input_words[num-3],
           word2==input_words[num-2],
           word3==input_words[num-1],
           word4==input_words[num])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 5)) %>%
        as.character() -> out
    ifelse(out=="character(0)", quadgram(input_words), return(out))
}

sextgram <- function(input_words){
    num <- length(input_words)
    filter(sext_words,
           word1==input_words[num-4],
           word2==input_words[num-3],
           word3==input_words[num-2],
           word4==input_words[num-1],
           word5==input_words[num])  %>%
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 6)) %>%
        as.character() -> out            
    ifelse(out=="character(0)", quintgram(input_words), return(out))
}

#Create User Input and Data Cleaning Function; Calls the matching functions

ngrams <- function(input_text){
    # Create a dataframe
    input_text <- data_frame(text = input_text)
    # Clean the Inpput
    replace_reg <- "[^[:alpha:][:space:]]*"
    input_text <- input_text %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # Find word count, separate words, lower case
    input_count <- str_count(input_text, boundary("word"))
    input_words <- unlist(str_split(input_text, boundary("word")))
    input_words <- tolower(input_words)
    # Call the matching functions
    nextWord <- ifelse(input_count == 0, "Please eneter your word or phrase in the given left text box.",
                       ifelse (input_count == 1, bigram(input_words),
                               ifelse (input_count == 2, trigram(input_words),
                                       ifelse (input_count == 3, quadgram(input_words),
                                               ifelse (input_count == 4, quintgram(input_words), sextgram(input_words))))))
    if(nextWord == "?"){
        nextWord = "The application not found the next expected word due to limited size of the training data" 
    }
    return(nextWord)
}

shinyServer(function(input, output){
    output$prediction <- renderPrint({
        next_word <- ngrams(input$inputString)
        next_word
    })
    
    output$text1 <- renderText({
        input$inputString
    });
})