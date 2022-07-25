ngram_fxn <-
function(n=c(1,2,3,4)){
  if(n==1){
    data_tbl %>%
      unnest_tokens(ngram, text)
  } else{
    data_tbl %>%
      unnest_tokens(ngram, text, token="ngrams", n=n) %>%
      filter(!is.na(ngram)) %>%
      separate(ngram, paste0("item", 1:n), sep=" ", remove=FALSE)
  }
}
