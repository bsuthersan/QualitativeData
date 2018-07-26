##Wordcloud script for qualitative data

library(tm)
library(SnowballC)
library(wordcloud2)

#Read the data and create it as a corpus

data <- readLines("https://raw.githubusercontent.com/bsuthersan/QualitativeData/master/interveiwdata.txt")
data <- Corpus(VectorSource(data))

#Basic text transformation

data <- tm_map(data, removeWords, stopwords("english"))
data <- tm_map(data,removePunctuation)
data <- tm_map(data, removeWords, c("YP","FM"))
data <- tm_map(data, removeNumbers)
data <- tm_map(data, content_transformer(tolower))
data <- tm_map(data, stripWhitespace)

#Process the data

dtm <- TermDocumentMatrix(data)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
