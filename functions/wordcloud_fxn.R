wordcloud_fxn <-
function(dat){
  wordcloud2(dat, color=brewer.pal(8, "Dark2"), size=0.5,
             fontFamily="Verdana", minRotation=0, maxRotation=0)
}
