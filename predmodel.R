### LOAD DATA AND LIBRARIES ###################################################

# libraries
library(tidyverse)
library(tidytext)
library(tm)
library(stringi)

# required functions
source("functions/freqngrams_fxn.R")
source("functions/inputclean_fxn.R")

# n-grams
unigram_df  <- readRDS("outputdata/unigram.rds")
bigram_df   <- readRDS("outputdata/bigram.rds")
trigram_df  <- readRDS("outputdata/trigram.rds")
quadgram_df <- readRDS("outputdata/quadgram.rds")



### DEVELOPING PREDICTORS #############################################

# get frequencies of n-grams
bigram_df   <- freqngrams_fxn(n=2, removestopwords=FALSE)
trigram_df  <- freqngrams_fxn(n=3, removestopwords=FALSE)
quadgram_df <- freqngrams_fxn(n=4, removestopwords=FALSE)

# convert n-gram frequency data to predictor tables
bigram_pred <- bigram_df %>%
  mutate(input=str_extract(ngram, "^([a-z]+)"), 
         prediction=str_extract(ngram, "([a-z]+)$")) %>%
  select(input, prediction)
trigram_pred  <- trigram_df %>%
  mutate(input=str_extract(ngram, "^([a-z]+ [a-z]+)"), 
         prediction=str_extract(ngram, "([a-z]+)$")) %>%
  select(input, prediction)
quadgram_pred <- quadgram_df %>%
  mutate(input=str_extract(ngram, "^([a-z]+ [a-z]+ [a-z]+)"), 
         prediction=str_extract(ngram, "([a-z]+)$")) %>%
  select(input, prediction)

# export predictor tables
saveRDS(bigram_pred,   "outputdata/bi_pred.rds")
saveRDS(trigram_pred,  "outputdata/tri_pred.rds")
saveRDS(quadgram_pred, "outputdata/quad_pred.rds")

# remove unnecessary variables
rm(freqngrams_fxn, bigram_df, trigram_df, quadgram_df)
gc()



### NEXT WORD PREDICTION ######################################################

# n-gram prediction function
predngram_fxn <- function(inputtext_tbl){
  predngram <- list(
    bigram   = inputtext_tbl %>% slice_tail(n=1) %>% unlist(),
    trigram  = inputtext_tbl %>% slice_tail(n=2) %>% unlist(),
    quadgram = inputtext_tbl %>% slice_tail(n=3) %>% unlist()
  )
  
  predngram %>%
    lapply(paste, collapse=" ")
}
dump(list="predngram_fxn", "functions/predngram_fxn.R")

# next word prediction function
nextword_fxn <- function(inputtext){
  input_clean <- tibble(text=inputclean_fxn(inputtext)) %>%
    unnest_tokens(item, text)
  
  predngrams <- predngram_fxn(input_clean)
  n_words <- nrow(input_clean)
  preds <- NULL
  
  if(n_words>=3 & (is.null(preds) | nrow(tibble(preds))<10)){
    if(is.null(preds)){
      preds <- quadgram_pred %>% 
        filter(input==predngrams$quadgram) %>%
        slice_head(n=10) %>%
        select(prediction) %>%
        unique()
    }
  }
  
  if(n_words>=2 & (is.null(preds) | nrow(tibble(preds))<10)){
    sub_pred <- trigram_pred %>% 
      filter(input==predngrams$trigram) %>%
      slice_head(n=10) %>%
      select(prediction) %>%
      unique()
    if(is.null(preds)){
      preds <- sub_pred
    } else{
      preds <- rbind(preds, sub_pred) %>%
        unique()
    }
  }
  
  if(n_words>=1 & (is.null(preds) | nrow(tibble(preds))<10)){
    sub_pred <- bigram_pred %>% 
      filter(input==predngrams$bigram) %>%
      slice_head(n=10) %>%
      select(prediction) %>%
      unique()
    if(is.null(preds)){
      preds <- sub_pred
    } else{
      preds <- rbind(preds, sub_pred) %>%
        unique()
    }
  }
  
  if(n_words>=0 & (is.null(preds) | nrow(tibble(preds))<10)){
    sub_pred <- unigram_df %>%
      slice_head(n=10) %>%
      unique()
    if(is.null(preds)){
      preds <- sub_pred
    } else{
      preds <- rbind(preds, sub_pred) %>%
        unique()
    }
  }
  
  preds
}
dump(list="nextword_fxn", "functions/nextword_fxn.R")


