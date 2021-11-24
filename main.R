## Set working directory
setwd("Your Directory")

## Packages
library(httpuv) # web authentication
library(lubridate) # date and time
library(tm)       # text mining
library(rtweet) # R client for twitter's api
library(plyr)   # for some 'aply'
library(stringr) # for 'str' functions
library(ggplot2) # for plotting/graphing
library(ggeasy) # for QoL graphing
library(dplyr)  # Grammar manipulation

## API Keys and Access Tokens
api_key <- "xxxxxxxxxxxxxxxx"
api_secret_key <- "xxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxx"

## Authenticate via direct authentication
token <- create_token(
  app = "Your Bot Name",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## To check if token matches
get_token()

## Extracting tweets
## 18k is the max value that 'rtweet' can extract from twitter.


## You can simply replace the hashtag that you want to extract


## a_tweets

a_tweet1 <- search_tweets("#a", n = 13000, include_rts = TRUE, lang = "en")
a_tweet2 <- search_tweets("#a", n = 13000, include_rts = TRUE, lang = "en")
  
## b_tweets

b_tweet1 <- search_tweets("#b", n = 13000, include_rts = TRUE, lang = "en")
b_tweet2 <- search_tweets("#b", n = 13000, include_rts = TRUE, lang = "en")


## Pre-processing the text (If ever needed)
purge_text = function(y){
  
  # To lower case
  y = tolower(y)
  
  # Using 'gsub' to take the input and substitute it again
  
  # To remove at (@)
  y = gsub("@\\w+", "", y)
  
  # To remove numbers
  y = gsub("[[:digit:]]", "", y)
  
  # To remove tabs
  y = gsub("[ |\t]{2,}", "", y)
  
  # To remove punctuation
  y = gsub("[[:punct:]]", "", y)
  
  # To remove links (http)
  y = gsub("http\\w+", "", y)
  
  # To remove blank spaces at the beginning
  y = gsub("^ ", "", y)
  
  #To remove blank spaces at the end
  y = gsub(" $", "", y)
  
  y = str_replace_all(y, "[^[:graph:]]", " ")
  return(y)
}

a_clean1 <- purge_text(a_tweet1)
a_clean2 <- purge_text(a_tweet2)

b_clean1 <- purge_text(b_tweet1)
b_clean2 <- purge_text(b_tweet2)


##Frequency of the tweets

# a_Frequency_Plot

a_tweet1 %>%
  
  # Interval
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #a tweets from the past week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

a_tweet2 %>%
  
  # Interval
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #a tweets from the past week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# b_Frequency_Plot

b_tweet1 %>%
  
  # Interval
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #b tweets from the past week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

b_tweet2 %>%
  
  # Interval
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #b tweets from the past week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) 

## Loading Sentiment word lists

positive_words = scan("Resources/positive-words.txt", what = "character", comment.char = ";")
negative_words = scan("Resources/negative-words.txt", what = "character", comment.char = ";")

# To extract the words
pos_extract = c(positive_words)
neg_extract = c(negative_words)

## Sentiment Scoring Function

sentiment_score = function(x, pos_extract, neg_extract){
  
  # 'laply' was used to combine the results into an array
  scores = laply(x, function(z, pos_extract, neg_extract){
    
    # Split into words
    word_list = str_split(z, "\\s+")
    
    # We want to convert the list into a vector
    words_unlisted = unlist(word_list)
    
    # Compare the words extracted (checking)
    positive_matches = match(words_unlisted, pos_extract)
    negative_matches = match(words_unlisted, neg_extract)
    
    # match() results + we want to make 'pos/neg_extract' a valid 'type'
    positive_matches = !is.na(positive_matches)
    negative_matches = !is.na(negative_matches)
    
    # Score
    score = sum(positive_matches) - sum(negative_matches)
    
    return(score)
    
  }, pos_extract, neg_extract)
  
  scores_df = data.frame(score = scores, text = sentences)
  
  return(scores_df)
}

## Calculating the Sentiment Score

# a_Analysis

a_analysis1 <- sentiment_score(a_clean1, pos_extract, neg_extract)
table(a_analysis1$score)

a_analysis2 <- sentiment_score(a_clean2, pos_extract, neg_extract)
table(a_analysis2$score)

# b_Analysis

b_analysis1 <- sentiment_score(b_clean1, pos_extract, neg_extract)
table(b_analysis1$score)

b_analysis2 <- sentiment_score(b_clean2, pos_extract, neg_extract)
table(b_analysis2$score)


## Histogram to show distribution of Sentiment tweets

# a_Histogram_Plot

a_analysis1 %>%
  ggplot(aes(x = score)) +
  geom_histogram(fill = "darkgray") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Sentiment Score", y = "Frequency",
    title = "#a Sentiment Score Distribution (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

a_analysis2 %>%
  ggplot(aes(x = score)) +
  geom_histogram(fill = "darkgray") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Sentiment Score", y = "Frequency",
    title = "#a Sentiment Score Distribution (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

# b_Histogram_Plot

b_analysis1 %>%
  ggplot(aes(x = score)) +
  geom_histogram(fill = "darkgray") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Sentiment Score", y = "Frequency",
    title = "#b Sentiment Score Distribution (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

b_analysis2 %>%
  ggplot(aes(x = score)) +
  geom_histogram(fill = "darkgray") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = "Sentiment Score", y = "Frequency",
    title = "#b Sentiment Score Distribution (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

## Barplot to show the sentiment type (Positive, Neutral, Negative)

# a_Sentiment_Barplot

positive <- length(which(a_analysis1$score > 0))
negative <- length(which(a_analysis1$score < 0))
neutral <- length(which(a_analysis1$score == 0))
sentiments <- c("Positive", "Neutral", "Negative")
count <- c(positive, neutral, negative)

# We want combine sentiment and count into a data frame, result
result <- data.frame(sentiments, count)

# factor() was used to categorize the strings from sentiment
result$sentiments <- factor(result$sentiments, levels = sentiments)
ggplot(result, aes(x = sentiments, y = count)) +
  
  # stat = identity was used to created a legend on the side
  geom_bar(stat = "identity", aes(fill = sentiments))+
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x ="Sentiments", y = "Count",
    title = "Barplot of #a tweet sentiments (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

positive <- length(which(a_analysis2$score > 0))
negative <- length(which(a_analysis2$score < 0))
neutral <- length(which(a_analysis2$score == 0))
sentiments <- c("Positive", "Neutral", "Negative")
count <- c(positive, neutral, negative)

# We want combine sentiment and count into a data frame, result
result <- data.frame(sentiments, count)

# factor() was used to categorize the strings from sentiment
result$sentiments <- factor(result$sentiments, levels = sentiments)
ggplot(result, aes(x = sentiments, y = count)) +
  
  # stat = identity was used to created a legend on the side
  geom_bar(stat = "identity", aes(fill = sentiments))+
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x ="Sentiments", y = "Count",
    title = "Barplot of #a tweet sentiments (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()

# b_Sentiment_Barplot

positive <- length(which(b_analysis1$score > 0))
negative <- length(which(b_analysis1$score < 0))
neutral <- length(which(b_analysis1$score == 0))
sentiments <- c("Positive", "Neutral", "Negative")
count <- c(positive, neutral, negative)

# We want combine sentiment and count into a data frame, result
result <- data.frame(sentiments, count)

# factor() was used to categorize the strings from sentiment
result$sentiments <- factor(result$sentiments, levels = sentiments)
ggplot(result, aes(x = sentiments, y = count)) +
  
  # stat = identity was used to created a legend on the side
  geom_bar(stat = "identity", aes(fill = sentiments))+
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x ="Sentiments", y = "Count",
    title = "Barplot of #b tweet sentiments (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()


positive <- length(which(b_analysis2$score > 0))
negative <- length(which(b_analysis2$score < 0))
neutral <- length(which(b_analysis2$score == 0))
sentiments <- c("Positive", "Neutral", "Negative")
count <- c(positive, neutral, negative)

# We want combine sentiment and count into a data frame, result
result <- data.frame(sentiments, count)

# factor() was used to categorize the strings from sentiment
result$sentiments <- factor(result$sentiments, levels = sentiments)
ggplot(result, aes(x = sentiments, y = count)) +
  
  # stat = identity was used to created a legend on the side
  geom_bar(stat = "identity", aes(fill = sentiments))+
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x ="Sentiments", y = "Count",
    title = "Barplot of #b tweet sentiments (EN)",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) + 
  ggeasy::easy_center_title()







  
  
 







