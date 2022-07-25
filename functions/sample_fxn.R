sample_fxn <-
function(dat, prob, seed=98765){
  set.seed(seed)
  sample(dat, size=length(dat)*prob)
}
