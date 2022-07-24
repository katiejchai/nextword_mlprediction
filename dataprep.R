### LOAD DATA AND LIBRARIES ###################################################

# libraries
library(tidyverse)
library(tidytext)
library(tm)
library(stringi)
library(wordcloud2)
library(RColorBrewer)
library(htmlwidgets)
library(webshot)

# hc corpus dataset
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
blogs_file   <- "data/en_US/en_US.blogs.txt"
news_file    <- "data/en_US/en_US.news.txt"
twitter_file <- "data/en_US/en_US.twitter.txt"

con <- file(blogs_file, open="r")
blogs <- readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)

con <- file(news_file, open="r")
news <- readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)

con <- file(twitter_file, open="r")
twitter <- readLines(con, encoding="UTF-8", skipNul=TRUE)
close(con)

# bad bad words dataset
# https://www.kaggle.com/datasets/nicapotato/bad-bad-words?resource=download
profanity <- read.csv("data/bad-words.csv", header=FALSE)[[1]]

# stop words dataset from tidytext package
data("stop_words")



### DATA SUMMARY ##############################################################

# create summary values
filename = filedir = file_size = num_lines = num_chars = num_words = 1:3
for (dataset in c("blogs", "news", "twitter")){
  if (dataset=="blogs")   {i = 1; dat <- blogs;   filedir[i] <- blogs_file}
  if (dataset=="news")    {i = 2; dat <- news;    filedir[i] <- news_file}
  if (dataset=="twitter") {i = 3; dat <- twitter; filedir[i] <- twitter_file}
  
  file_size[i] <- file.size(filedir[i])/(2^20)
  num_lines[i] <- length(dat)
  num_chars[i] <- sum(nchar(dat))
  num_words[i] <- sum(sapply(str_split(dat, "\\w+"), length))
}

# make data summary df
data_summary <- data.frame(
  file_name = c("blogs", "news", "twitter"),
  file_size, num_lines, num_chars, num_words
)

# export data summary
saveRDS(data_summary, file="outputdata/data_summary.rds")

# remove unnecessary variables
rm(blogs_file, news_file, twitter_file, con, filename, filedir, 
   file_size, num_lines, num_chars, num_words, dataset, i, dat)
gc()



### SAMPLE DATA ###############################################################

# define sampling function
sample_fxn <- function(dat, prob, seed=98765){
  set.seed(seed)
  sample(dat, size=length(dat)*prob)
}
dump(list="sample_fxn", file="functions/sample_fxn.R")

# make samples
blogs_sample   <- sample_fxn(blogs, 0.08)
news_sample    <- sample_fxn(news, 0.20)
twitter_sample <- sample_fxn(twitter, 0.02)
data_sample    <- c(blogs_sample, news_sample, twitter_sample)

# export sampled data
saveRDS(data_sample, "outputdata/data_sample.rds")

# remove unnecessary variables
rm(blogs, news, twitter, sample_fxn, 
   blogs_sample, news_sample, twitter_sample, data_summary)
gc()



### CLEAN DATA ################################################################

# input clean function
inputclean_fxn <- function(inputtext){
  # basic cleaning
  input_clean <- inputtext %>%
    tolower(.) %>% # all lowercase
    stri_trans_general(., id="Latin-ASCII") %>% # replace accented characters
    str_remove_all(., "http[^[:space:]]*") %>% # remove urls
    str_remove_all(., "[^[a-z ]]*") %>% # remove non alphabet/space chars
    str_replace_all(., "\\s+", " ") %>% # remove extra between-word whitespaces
    trimws(.) # remove leading/trailing whitespaces
  
  input_clean
}
dump(list="inputclean_fxn", "functions/inputclean_fxn.R")

data_tbl <- tibble(text=inputclean_fxn(data_sample))

# clean stop words dataset
stop_words <- stop_words %>%
  filter(lexicon=="SMART") %>% # largest set of stop words
  mutate(word = str_remove_all(tolower(word), "[^[a-z ]]")) %>%
  select(word) %>%
  unlist()

# remove unnecessary variables
rm(data_sample, inputclean_fxn)
gc()



### N-GRAMS ###################################################################

# tokenizing function
ngram_fxn <- function(n=c(1,2,3,4)){
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
dump(list="ngram_fxn", file="functions/ngram_fxn.R")

# remove n-grams with bad words
# unigram
unigram_df  <- ngram_fxn(n=1) %>%
  filter(!(ngram %in% profanity))

# bigram
bigram_df   <- ngram_fxn(n=2) %>%
  filter(!(item1 %in% profanity), !(item2 %in% profanity))

# trigram
trigram_df  <- ngram_fxn(n=3) %>%
  filter(!(item1 %in% profanity), !(item2 %in% profanity), 
         !(item3 %in% profanity))

# quadgram
quadgram_df <- ngram_fxn(n=4) %>%
  filter(!(item1 %in% profanity), !(item2 %in% profanity), 
         !(item3 %in% profanity), !(item4 %in% profanity))

# export n-grams
saveRDS(unigram_df,  "outputdata/unigram.rds")
saveRDS(bigram_df,   "outputdata/bigram.rds")
saveRDS(trigram_df,  "outputdata/trigram.rds")
saveRDS(quadgram_df, "outputdata/quadgram.rds")

# remove n-grams with stop words
# unigram
uni_nostop  <- unigram_df %>%
  filter(!(ngram %in% stop_words))

# bigram
bi_nostop   <- bigram_df %>%
  filter(!(item1 %in% stop_words), !(item2 %in% stop_words))

# trigram
tri_nostop  <- trigram_df %>%
  filter(!(item1 %in% stop_words), !(item2 %in% stop_words), 
         !(item3 %in% stop_words))

# quadgram
quad_nostop <- quadgram_df %>%
  filter(!(item1 %in% stop_words), !(item2 %in% stop_words), 
         !(item3 %in% stop_words), !(item4 %in% stop_words))

# remove unnecessary variables
rm(ngram_fxn, profanity, stop_words)
gc()



### EXPLORATORY PLOTS #########################################################

# n-gram frequency data function
freqngrams_fxn <- function(n=c(1,2,3,4), n_words=NULL, removestopwords=TRUE){
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
dump(list="freqngrams_fxn", file="functions/freqngrams_fxn.R")

# wordcloud function
wordcloud_fxn <- function(dat){
  wordcloud2(dat, color=brewer.pal(8, "Dark2"), size=0.5,
             fontFamily="Verdana", minRotation=0, maxRotation=0)
}
dump(list="wordcloud_fxn", file="functions/wordcloud_fxn.R")

# bar plot function
barplot_fxn <- function(dat){
  ggplot(dat, aes(x=count, y=factor(ngram, levels=rev(ngram)))) +
    geom_bar(stat="identity", fill="slategray", color="black") +
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
}
dump(list="barplot_fxn", file="functions/barplot_fxn.R")

# make wordclouds
uni_wc  <- wordcloud_fxn(freqngrams_fxn(n=1, n_words=100))
bi_wc   <- wordcloud_fxn(freqngrams_fxn(n=2, n_words=100))
tri_wc  <- wordcloud_fxn(freqngrams_fxn(n=3, n_words=80))
quad_wc <- wordcloud_fxn(freqngrams_fxn(n=4, n_words=20))

# make bar plots
uni_bp  <- barplot_fxn(freqngrams_fxn(n=1, n_words=10))
bi_bp   <- barplot_fxn(freqngrams_fxn(n=2, n_words=10))
tri_bp  <- barplot_fxn(freqngrams_fxn(n=3, n_words=10))
quad_bp <- barplot_fxn(freqngrams_fxn(n=4, n_words=10))

# save plots function
plotexport_fxn <- function(plotname, plottype=c("wc","bp"), 
                           filename, filetype=".png"){
  if(plottype=="wc"){
    htmlwidgets::saveWidget(widget=plotname,"temp.html", selfcontained=FALSE)
    webshot("temp.html", paste0("figures/", filename, filetype), 
            vwidth=1000, vheight=800, delay=30)
    
    unlink("temp_files", recursive=TRUE)
    unlink("temp.html")
  }
  if(plottype=="bp"){
    png(paste0("figures/", filename, filetype), width=600, height=600)
    print(plotname)
    dev.off()
  }
}
dump(list="plotexport_fxn", file="functions/plotexport_fxn.R")

# export wordclouds
plotexport_fxn(uni_wc,  plottype="wc", "uni_wordcloud")
plotexport_fxn(bi_wc,   plottype="wc", "bi_wordcloud")
plotexport_fxn(tri_wc,  plottype="wc", "tri_wordcloud")
plotexport_fxn(quad_wc, plottype="wc", "quad_wordcloud")

# export bar plots
plotexport_fxn(uni_bp,  plottype="bp", "uni_barplot")
plotexport_fxn(bi_bp,   plottype="bp", "bi_barplot")
plotexport_fxn(tri_bp,  plottype="bp", "tri_barplot")
plotexport_fxn(quad_bp, plottype="bp", "quad_barplot")

# remove remaining variables
rm(list=ls())
gc()

