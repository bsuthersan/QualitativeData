####Politics sentinmnet analysis

library(tidyverse)
library(tidytext)
library(stringr)

politics <- read_csv("https://raw.githubusercontent.com/bsuthersan/QualitativeData/master/Politics/politics.csv")

#Remove the mod

politics <- politics %>%
  filter(!str_detect(comment, "As a reminder, this subreddit"))

nrc <- get_sentiments("bing")

politics <- politics %>%
  select(title, post_date, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(stop_words, by = "word") %>%
  left_join(nrc, by = "word") %>%
  filter(!is.na(sentiment))

##Basic sentiment analysis

positive = c("positive", "trust","joy")
neutral = c('surprise','anticipation')

politics %>%
group_by(sentiment) %>%
  summarise(Total = n()) %>%
  mutate(Percent = Total/sum(Total)*100,
         Type = case_when(
           sentiment %in% positive ~ "Positive",
           sentiment %in% neutral ~ "Neutral",
            TRUE ~ "Negative")) %>%
  ggplot(aes(reorder(sentiment,Percent), Percent)) +
  geom_col(aes(fill=Type)) +
  coord_flip() +
  xlab("") + 
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("steelblue","gold","grey"))
  



