### preliminary and sentiment analysis

library(rtweet)
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

# library(scales) #for scales in graphs
# library(reactable) #for tables
library(lubridate) #for dates
# library(textdata) #to get dictionaries for sentiment analysis
# library(wordcloud2) #for wordcloud
# library(wordcloud) #for comparison cloud
# library(reshape2) #to change shape of df

# Importing tweets
tw_original <- read_twitter_csv("C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media.csv")

tw_original %>%
  group_by(name) %>%
  summarize(n = n())
"
Considering the most popular media outlets in the US, I randomly picked five major newspapers to compare
how different their timelines in Twitter is from one another. Financial Times is London-based, however
it usually gives a large coverage of news related to the US. Moreover, the idea is to find how different
is the content of the timelines from newspapers, which are known for their political reporting, from the 
ones that are known for their focus on business and economy. In this sense, I decided to classify these
newspapers based on my own impression about the focus of each media outlet. Therefore, 'The New York Times' 
and 'The Washington Post' are considered as the politics-focused, whereas 'Financial Times' and
'The Wall Street Journal' are business-focused.
"

# Filtering tweets and creating equal timeline
tw_media <- tw_original %>%
  select(name, created_at, text, source, display_text_width, is_retweet,
         favorite_count, retweet_count, lang, geo_coords, status_id) %>%
  #filter(name != 'Bloomberg') %>%
  mutate(date = as_datetime(created_at)) %>%
  group_by(name) %>%
  #mutate(diff = max(date) - min(date)) %>%
  mutate(diff = max(date) - date) %>%
  filter(date > ymd_hms("2020-09-21 16:18:06")) %>%
  select(name, date, diff)

?as_date
tw_media %>%
  group_by(name) %>%
  summarize(n = n())

tw_media %>%
  select(name, created_at) %>%
  filter(name == "The Wall Street Journal") %>%
  arrange(created_at)

"
Initially, I also considered to use the timeline from Bloomberg, but decided to exclude it from the analysis
after checking the timeline of their tweets. The frequency of new tweets is higher than the rest of newspapers.
This implies that the retrieved tweets from Bloomberg only cover a little less than two weeks, whereas the rest
of newspapers go over a month.
"