library(tm)
library(tidytext)
library(tidyverse)
library(wordcloud2)

setwd("~/Documents/Repositories")

data <- VCorpus(DirSource("Obama data"))

obamadata <- tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Year, word) %>%
  summarise(Total = n())
  
  

  