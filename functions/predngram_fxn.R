predngram_fxn <-
function(inputtext_tbl){
  predngram <- list(
    bigram   = inputtext_tbl %>% slice_tail(n=1) %>% unlist(),
    trigram  = inputtext_tbl %>% slice_tail(n=2) %>% unlist(),
    quadgram = inputtext_tbl %>% slice_tail(n=3) %>% unlist()
  )
  
  predngram %>%
    lapply(paste, collapse=" ")
}
