##Scrape and process the reddit data

library(RedditExtractoR)

politics <- get_reddit(subreddit = "politics", page_threshold = 20)

