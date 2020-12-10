### preliminary and sentiment analysis

library(rtweet)
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

library(scales) #for scales in graphs
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
  filter(lang == "en") %>%
  select(name, date, text, source, is_retweet, favorite_count, #201210
         retweet_count, lang, geo_coords, status_id)
  
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
# checking for retweets
tw_media %>%
  group_by(name, is_retweet) %>%
  count()

"
I considered deleting retweets, but due to the difference between the four outlets, I decided to keep them.
Even though some tweets will repeat themselves, the point is too identify what kind of news where given more
attention in the timeline and that includes retweets as well. Moreover, there are some outlets that tweet the
same reports several times, but are not marked as retweets.
"
"
There are some things that should be considered in this part. I didn't exclude
retweets as I plant to see the timeline as a whole. Usually, a tweet that starts with ' are deleted, because
they are considered retweets. However, some tweets start with a quote and they are not necessarily a retweet.
"
# getting tokens (words)
#[^A-Za-z\\d#@'] --> it means to match a single that is NOT in a letter (A-Za-z), a symbol(\, #, @) or a number (d)
#'(?![A-Za-z\\d]) --> ?! negative lookahead. it means check letter or number before '
#^' --> to check if ' at the beginning of a tweet
#reg <- ([^A-Za-z\\d#@\\-']|'(?![A-Za-z\\d])|^'|'s)
#reg <- "([^A-Za-z\\d#@']|'(\\w{2})|'(\\s|$)|'s)"
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d])|'s|^'|(?<=\\s)')"

tw_words <- tw_media %>%
  #filter(is_retweet == FALSE) %>% #checking for retweets
  select(-geo_coords, -favorite_count, -retweet_count) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(words, text, token = "regex", pattern = reg) %>% #pattern here says: tokenize everything that is in reg
  filter(!words %in% stop_words$word, #word not in stop_words
         str_detect(words, "[a-z]"))

# 10 most common words
tw_words %>%
  count(words) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(reorder(words,n), n)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
  ggtitle("Ten most common words") +
  xlab("Words") +
  ylab("n") +
  theme_light()

"
The first word  appears almost two times more than the second most common word. At first glance, it looks
like all the words relate between one another, with two distinguishable topics: US election and the pandemic.
"

### timeline
tw_media %>%
  #filter(is_retweet == FALSE) %>%
  ggplot(aes(as.Date(date), fill = name)) +  
  geom_histogram(position = 'identity', binwidth = 1, show.legend = FALSE, alpha = 0.65) +
  geom_histogram(data = subset(tw_media %>%
                                 #filter(is_retweet == FALSE) %>%
                                 mutate(wknds = wday(date)), wknds==7 | wknds==1), 
                 binwidth = 1, alpha = 0.99, show.legend = FALSE) +
  ggtitle("Frequency of tweets by day") +
  xlab("Date") +
  ylab("Count") +
  facet_wrap(.~ name, ncol = 1) +
  theme_light()
"
As for the frequency, it looks like the outlets all share an almost uniform distribution with some
exceptions (fridays). The darker areas represent the weekends with fewer tweets in comparison to weekdays.
When removing retweets, the distribution remains the same, which suggests that retweets are more or less
evenly distributed along the timeline.
"

### rate
tw_media %>%
  mutate(date = as_date(date)) %>%
  group_by(name, date) %>%
  mutate(n = n()) %>%
  ggplot(aes(date, n, color = name)) +
  geom_line(size = 1) +
  ggtitle("Frequency of tweets by day") +
  xlab("Date") +
  ylab("Count") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

"
Visually, it looks like the two politics-focused media outlets have a higher
average rate of tweets on weekdays than the business-focused outlets. This 
difference is more evident towards the end of the week
"
### daytime
tw_media %>%
  count(name, hour = hour(with_tz(date, 'EST'))) %>% #smotret' perviy i udalit' lubridate
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, n, fill = name)) +
  geom_bar(stat = 'identity', show.legend = FALSE, alpha = 0.5) +
  geom_bar(data = subset(tw_media %>%
                           filter(is_retweet == FALSE) %>%
                           count(name, hour = lubridate::hour(with_tz(date, 'EST'))) %>%
                           mutate(percent = n / sum(n))),
           stat = 'identity', show.legend = FALSE) +
  scale_x_continuous(breaks = unique(hour(with_tz(tw_media$date, 'EST')))) +
  ggtitle("Time of the day for tweets (EST)") +
  xlab("Hour") +
  ylab("Count") +
  facet_wrap(. ~ name, ncol = 1) +
  theme_light()

"
For The New Yortk Times and The Washington Post is easier to distinguish the time of
the day, where more tweets are published, than for the other two outlets. It is important
to consider that the time for each tweet is adjusted for EST. Besides, the Financial Times
is a London-based newspaper, which is why it follows a different pattern. Moreover, it is 
also worth noting that the darker color represents the timeline without retweets, whereas the light
color are the added retweets. This shows that the time of the day with more tweets is also
the period with more retweets.
"

# line plot
tw_media %>%
  count(name, hour = hour(with_tz(date, 'EST'))) %>%
  group_by(name) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = name)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  xlab("Hour") +
  ylab("%")

### lenght and number of words
# length of tweet
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(length = nchar(text)) %>%
  ggplot(aes(x=name, y=length, fill = name)) +
  geom_boxplot(show.legend = FALSE)


test <- tw_media %>%
  #filter(is_retweet == FALSE) %>% 
  filter(name == "Financial Times") %>%
  select(-geo_coords, -favorite_count, -retweet_count) %>%
  #mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(length = nchar(text)) %>%
  arrange(desc(display_text_width)) %>%
  #group_by(name) %>%
  ggplot(aes(x=status_id, y=length)) +
  #geom_histogram()
  geom_bar(stat = 'identity')
  #geom_boxplot(aes(name, length, fill = name))




?percent_format

#marlon puerta


tw_media %>%
  select(name, date) %>%
  filter(name == "The Wall Street Journal") %>%
  arrange(desc(date))

test <- tw_words %>%
  filter(str_detect(word, '"')) %>%
  arrange(desc(word))