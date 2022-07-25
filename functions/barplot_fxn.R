barplot_fxn <-
function(dat){
  ggplot(dat, aes(x=count, y=factor(ngram, levels=rev(ngram)))) +
    geom_bar(stat="identity", fill="slategray", color="black") +
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
}
