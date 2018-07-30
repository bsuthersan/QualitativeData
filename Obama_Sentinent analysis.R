library(tm)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(ggthemes)

data <- VCorpus(DirSource("Obama data"))

filterwords <- c("ca","d0","d5","d3","d2","applause", "ve")

tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, Year, sort=TRUE) %>%
  filter(!(word %in% filterwords)) %>%
  mutate(word = case_when(
    word=="americans" ~ "american",
    word=="jobs" ~ "job",
    TRUE ~ word
  )) %>%
  group_by(Year) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, -n)) %>%
  ggplot(aes(x = word, y= n, fill=Year)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~Year, scales = "free_y") +
  coord_flip() +
  theme_fivethirtyeight()
  
  
  
  

  