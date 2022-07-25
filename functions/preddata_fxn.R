preddata_fxn <-
function(){
  unigram_df    <- readRDS("outputdata/unigram.rds")
  bigram_pred   <- readRDS("outputdata/bi_pred.rds")
  trigram_pred  <- readRDS("outputdata/tri_pred.rds")
  quadgram_pred <- readRDS("outputdata/quad_pred.rds")
}
