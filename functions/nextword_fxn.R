nextword_fxn <-
function(inputtext){
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
