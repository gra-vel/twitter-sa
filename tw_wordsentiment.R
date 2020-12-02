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

# Filtering tweets and defining time period
tw_media <- tw_original %>%
  mutate(date = as_datetime(created_at)) %>%
  select(name, date, text, source, display_text_width, is_retweet,
         favorite_count, retweet_count, lang, geo_coords, status_id)
  
tw_media %>%
  group_by(name) %>%
  mutate(n = n(),
         initial = min(date),
         diff = max(date) - min(date)) %>%
  select(name, n, initial, diff) %>% 
  unique()

"
Initially, I also considered to use the timeline from Bloomberg, but decided to exclude it from the analysis
after checking the timeline period of their tweets. It appears that the frequency of new tweets is higher in 
comparison to the rest of newspapers. This implies that the retrieved tweets from Bloomberg only cover a 
little less than two weeks, whereas the rest of newspapers go over a month. Therefore, I decided to remove 
those tweets. I also limited the time period to go from the first tweet of the Wall Street Journal timeline to
the date these tweets were retrieved. It is important to consider that the defined period will not cover the 
timeline from The Washington Post by almost a day and The New York Times by more than three days.
"  
tw_media <- tw_media %>%
  filter(name != 'Bloomberg',
         date >= min(date[which(name == "The Wall Street Journal")])) #zdes' mozhno videt' kak rabotayet s drugimi variantami

tw_media %>%
  group_by(name) %>%
  summarise(n = n())

"
The defined time period will cover tweets from 2020-09-21 until 2020-10-26.
"
### Lexicon-based sentiment analyisis
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@])|^'|'s)"

tw_words <- tw_media %>%
  filter(is_retweet == FALSE) %>%
  #erases tweets beginning with quotes. problem is some tweets begin with some name in quotes. tweet itself is not a quote
  #maybe erase rt before?
  #filter(!str_detect(text, '^"')) %>%
  select(-geo_coords, -favorite_count, -retweet_count) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>% #pattern here says: tokenize everything that is in reg
  filter(!word %in% stop_words$word, #word not in stop_words
         str_detect(word, "[a-z]"))


#marlon puerta


tw_media %>%
  select(name, date) %>%
  filter(name == "The Wall Street Journal") %>%
  arrange(desc(date))

