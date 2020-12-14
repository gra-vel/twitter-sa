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
         !str_detect(words, "^000"),
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
# number of total words
tw_words %>%
  group_by(name) %>%
  count(words) %>%
  mutate(total_words = sum(n),
         unique_words = n()) %>%
  select(name, total_words, unique_words) %>% unique() %>%
  ggplot(aes(x = name, y = total_words,  fill = name)) +
  geom_bar(stat = 'identity', show.legend = FALSE, alpha = 0.6) +
  geom_bar(aes(y = unique_words), stat = 'identity', show.legend = FALSE) +
  geom_text(aes(label = total_words), nudge_y = -1000) +
  geom_text(aes(y = unique_words, label = unique_words), nudge_y = -1000) +
  ggtitle('Number of total and unique words in tweets') +
  xlab('') +
  ylab('Word count') +
  theme_light()

"
With the exception of FT, all outlets have around 3200 tweets. The NYT is the one with the most 
words followed by the WSJ. The darker color shows the numnber of unique words used in each timeline
At this level, there is less variability than in the count for total words.
"

# more used words by media
tw_words %>% 
  group_by(name) %>%
  count(words) %>%
  top_n(10) %>%
  ggplot(aes(reorder_within(words, n, name), n, fill = name)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(. ~ name, scales = "free") +
  ggtitle('Most frequent used word by media outlet') +
  ylab('Word count') +
  xlab('Words') +
  theme_light()
  
"
For these four newspapers, the most used word in this period is 'trump', although it is 
noticeable that this word has a bigger difference than the rest of words for the 
politics-focused outlets, than for the economics-focused outlets. Nonetheless, it is also
clear that the words used by the all of these outlets remain similar. It is clear that the
highlight in this period was around the US election and the pandemic. Also noteworthy is the
fact that FT and the WSJ use 'covid' more often than 'coronavirus', in contrast to the NYT
and the WP.
"

### word frequencies as proportion
# Business-focused
frequency_eco <- tw_words %>%
  filter(!name %in% c("The New York Times", "The Washington Post")) %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  gather(name, proportion, 'The Wall Street Journal')

frequency_eco %>%
  filter(proportion > 0.00025,
         `Financial Times` > 0.00025) %>%
  ggplot(aes(proportion, `Financial Times`, color = abs(`Financial Times` - proportion))) +
  geom_abline(color = "black") +
  #geom_jitter(color = "darkgreen", alpha = 0.1, size = 3, width = 0.3, height = 0.3, show.legend = FALSE) + 
  #geom_text(aes(label = words), check_overlap = TRUE, vjust = 1.5) +
  geom_point(color = "red", alpha = 0.2, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "black", high = "black") +
  theme_light() 
  #theme(legend.position = "none")

# Politics-focused
frequency_pol <- tw_words %>%
  filter(name %in% c("The New York Times", "The Washington Post")) %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  gather(name, proportion, 'The Washington Post')

frequency_pol %>%
  filter(proportion > 0.00025,
         `The New York Times` > 0.00025) %>%
  ggplot(aes(proportion, `The New York Times`, color = abs(`The New York Times` - proportion))) +
  geom_abline(color = "black") +
  geom_point(color = "red", alpha = 0.2, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "black", high = "black") +
  theme_light()

# Total
frequency_total <- tw_words %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  gather(name_eco, proportion_eco, c('Financial Times','The Wall Street Journal')) %>%
  gather(name_pol, proportion_pol, c('The New York Times', 'The Washington Post'))

frequency_total %>%
  #filter(proportion_eco > 0.00025,
  #       proportion_pol > 0.00025) %>%
  ggplot(aes(proportion_eco, proportion_pol, color = abs(proportion_eco - proportion_pol))) +
  geom_abline(color = "black") +
  geom_point(color = "red", alpha = 0.2, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 1), #check this scale
                       low = "black", high = "black") +
  facet_wrap(name_eco~name_pol)

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
  ylab("%") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

### lenght and number of words
# length of tweet
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(length = nchar(text)) %>%
  ggplot(aes(x=name, y=length, fill = name)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("Tweet length") +
  xlab("") +
  ylab("Length") +
  theme_light()

"In terms of lenght, the NYT has a higher median than the rest of outlets. The WP has the lowest median
and a group of outliers at both ends of the distribution."

#distribution accordng status_id
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  mutate(length = nchar(text)) %>%
  #arrange(desc(display_text_width)) %>%
  ggplot(aes(x=status_id, y=length)) +
  geom_bar(stat = 'identity') +
  facet_wrap(.~name, ncol = 1, scales = "free_x")
  








?percent_format

#marlon puerta


tw_media %>%
  select(name, date) %>%
  filter(name == "The Wall Street Journal") %>%
  arrange(desc(date))

test <- tw_words %>%
  filter(str_detect(word, '"')) %>%
  arrange(desc(word))