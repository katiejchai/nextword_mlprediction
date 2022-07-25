inputclean_fxn <-
function(inputtext){
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
