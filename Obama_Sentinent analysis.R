library(tm)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(ggthemes)
library(wordcloud2)
library(topicmodels)

data <- VCorpus(DirSource("Obama data"))

##Build the filterwords

filterwords <- c("ca","d0","d5","d3","d2","applause", "ve")

##Process the data

wordclouddata <- tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% filterwords)) %>%
  mutate(word = case_when(
    word=="americans" ~ "american",
    word=="jobs" ~ "job",
    word=="businesses" ~ "business",
    TRUE ~ word
  )) %>%
  count(word, sort = TRUE)

##Build the wordcloud
colorVec = rep(c("#28201C","#D9A638","#159A92","#9A2027"), length.out = nrow(new))
wordcloud2(wordclouddata, minSize=25, color= colorVec, shape = "circle", fontFamily = "sans")

##Part 2: Time analysis

obamayears <- tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% filterwords)) %>%
  mutate(word = case_when(
    word=="americans" ~ "american",
    word=="jobs" ~ "job",
    word=="businesses" ~ "business",
    TRUE ~ word
  )) %>%
  count(word, Year, sort=TRUE) %>%
  group_by(Year) %>%
  top_n(20) %>%
  ungroup()

##Plot

ggplot(obamayears, aes(x = reorder(word, n), y= n, fill=Year)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~Year, scales = "free_y") +
  coord_flip() +
  xlab("") +
  ylab("")

##Identify which ones are repeats
##So, basically, Obama often repeats himself

obamarepeats <- obamayears %>%
  group_by(word) %>%
  summarise(Total = n()) %>%
  filter(Total>1)

obamayears %>%
  mutate(Unique = case_when(
    word %in% obamarepeats$word ~ "Repeat theme",
    TRUE ~ "Unique"
  )) %>%
  ggplot(aes(word, n, fill=Unique)) +
  geom_col() +
  facet_wrap(~Year, scales = "free_y") +
  coord_flip() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c("forestgreen","steelblue"))

##Yup, Obama repeats himself A LOT.

##Finally, let's look at the sentiments that Obama is expressing
##In each of his speeches

nrc <- get_sentiments("nrc")

 tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% filterwords)) %>%
  left_join(nrc, by = "word") %>%
  filter(!is.na(sentiment)) %>%
  group_by(Year, sentiment) %>%
  summarise(Total = n()) %>%
  mutate(Percent = Total/sum(Total)*100) %>%
  ggplot(aes(sentiment, Percent, fill=Year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Year, ncol = 4, scales = "free_y") +
  coord_flip()
  
##Part three: topic modelling

##Create the data as a DocumentTermMatrix
##But clean first

data <- VCorpus(DirSource("Obama data"))
 
newdata <- tidy(data) %>%
  mutate(Year = gsub(".txt","", id)) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% filterwords)) %>%
  count(Year, word) %>%
  cast_dtm(Year, word, n)

new <- LDA(newdata, k=3, control = list(seed=1234))

newagain <- tidy(new, matrix="beta")

ap_top_terms <- newagain %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##Get the most relevant by year




