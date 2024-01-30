library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud)



df <- read.csv('bfro_reports_geocoded.csv')
observed <- df$observed
observed <- dplyr::tibble(text = observed)

words <-observed %>% unnest_tokens(word,text)

custom_stop_words <- tribble(~word, ~lexicon,
                             "HERE GOES A CUSTOM STOP WORD", "CUSTOM")

stop_words_complete <- stop_words %>% bind_rows(custom_stop_words)


words <- words %>% 
  anti_join(stop_words_complete)%>%
  count(word) %>% 
  arrange(desc(n))

wordcloud(words = words$word,
          freq = words$n,
          max.words = 20,
          colors = "red")