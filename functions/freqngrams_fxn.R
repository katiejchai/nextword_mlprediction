freqngrams_fxn <-
function(n=c(1,2,3,4), n_words=NULL, removestopwords=TRUE){
  if(removestopwords==TRUE){
    if(n==1){dat <- uni_nostop}; if(n==2){dat <- bi_nostop}
    if(n==3){dat <- tri_nostop}; if(n==4){dat <- quad_nostop}
  } 
  else{
    if(n==1){dat <- unigram_df}; if(n==2){dat <- bigram_df}
    if(n==3){dat <- trigram_df}; if(n==4){dat <- quadgram_df}
  }
  
  dat %>%
    filter(!is.na(ngram)) %>%
    count(ngram, name="count") %>%
    arrange(desc(count)) %>%
    slice_head(n=ifelse(is.null(n_words), nrow(dat), n_words))
}
