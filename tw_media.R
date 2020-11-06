library(rtweet) #for tweet import
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

library(scales) #for scales in graphs
library(reactable) #for tables
library(lubridate) #for dates

# WSJ - @WSJ, NYT - @nytimes, Bloomberg - @business, FT - @FinancialTimes, The Washington Post - @washingtonpost
### api keys
create_token(app = 'tw_recent',
             consumer_key = "Pfa0ShU6CEbgBescgHn1zFSot",
             consumer_secret = "inkW4taU6nxkQPAtBV0laDLWgnK09PJVm2Vl7BFnPUhg8b7um8",
             access_token = "1264947985458266112-qlPBFaRumU7WPwViuXTf62qIAnAwpU",
             access_secret = "1MakaYhPygWVI1e9tSjKawURNFrvdFkRTXsnXY7eZbazH")

### retrieve data
# tw_retrieve <- get_timeline(c("WSJ","nytimes","business","FinancialTimes","washingtonpost"), n=3200)
# tw_retrieve2 <- get_timeline_unlimited(c("WSJ","nytimes","business","FinancialTimes","washingtonpost"), n=5000)
# write_as_csv(tw_retrieve, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media.csv")
# write_as_csv(tw_retrieve2, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media2.csv")
tw_retrieve <- read_twitter_csv("C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media.csv")
tw_retrieve2 <- read_twitter_csv("C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media2.csv")

#here I could create a nice table from the statistics book
names(tw_retrieve)
tw_retrieve %>%
  group_by(screen_name) %>%
  summarize(n = n())

### selecting variables
#display_text width - changes in length?
#favorite_count / retweet_count - check which are enabled and disabled, what's common there?
##hashtags - don't know if i needed, maybe i can check with regex
#lang - why are there other languages
tw_media <- tw_retrieve %>%
  select(name, created_at, text, source, display_text_width, is_retweet,
         favorite_count, retweet_count, lang, geo_coords)

### clean
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@])|^'|'s)" #this term erases everything, except ' when it's in the middle of a word (it's, don't)
#([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@])) #everything that is not a character, number, hashtag, @
#([^A-Za-z\\d#@']|'(?=[A-Za-z\\d#@])|'(?![A-Za-z\\d#@])) #this term would erase everything that is not a character. the two additional terms (after |) erases ' at the every position
tw_words <- tw_media %>%
  filter(is_retweet == FALSE) %>%
  #erases tweets beginning with quotes. problem is some tweets begin with some name in quotes. tweet itself is not a quote
  #maybe erase rt before?
  #filter(!str_detect(text, '^"')) %>%
  select(-geo_coords) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>% #pattern here says: tokenize everything that is in reg
  filter(!word %in% stop_words$word, #word not in stop_words
         str_detect(word, "[a-z]")) #erases numbers
  
tw_words %>%
  #group_by(name) %>%
  count(word) %>%
  #arrange(n) %>%
  arrange(desc(n)) %>%
  #filter(name == "The Wall Street Journal") %>%
  top_n(10) %>%
  ggplot(aes(reorder(word,n), n)) +
  geom_bar(stat = "identity")

### timeline
ggplot(tw_media, aes(x = as.Date(created_at), fill = name)) +
  geom_histogram(position = 'identity', binwidth = 1, show.legend = FALSE) +
  facet_wrap(. ~ name, ncol = 1)
"
FT goes as back as the beginning of september. weekends are more noticeable here
NYT, WSJ and WP have more or less the same range, going as back as the third week of september
Bloomberg has a bigger rate than all of the other. concentrated in two last weeks
"
### rate
tw_media %>%
  mutate(day_s = as_date(created_at)) %>%
  group_by(name, day_s) %>%
  mutate(n = n()) %>% 
  ggplot(aes(day_s, n, color = name)) +
  geom_line() #for shades, the plot in baseball statistics

tw_media %>%
  mutate(day_s = as_date(created_at)) %>%
  group_by(name, day_s) %>%
  summarise(n = n(), .groups = 'drop')

"
Bigger rate in Bloomberg. For the rest of newspapers there is a quite similar trend
with weekends showing less tweets.
"

### daytime
# bar plot
tw_media %>%
  count(name, hour = lubridate::hour(with_tz(created_at, 'EST'))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, n, color = name)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ name, ncol = 1)
  
# line plot
tw_media %>%
  count(name, hour = lubridate::hour(with_tz(created_at, 'EST'))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, n, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day",
       y = "% of tweets",
       color = "")

"
In a bar plot is easier to see the behavior and trends of each media. However,
with the line plot is easier to compare. First may be more appropriate, because
FT is in a different time zone, although the distribution looks uniform.
"

### lenght and number of words
# length of tweet
tw_media %>%
  group_by(name) %>%
  ggplot() +
  geom_boxplot(aes(name, display_text_width))

"
box plot is interesting, but it needs less outliers
"

# number of total words
tw_words %>% 
  group_by(name) %>%
  mutate(words_total = n()) %>%
  select(name, words_total) %>% unique() %>%
  ggplot() +
  geom_bar(aes(name, words_total), stat = "identity")

# number of unique words
tw_words %>% 
  group_by(name) %>%
  count(word) %>%
  mutate(words_total = n()) %>%
  select(name, words_total) %>% unique() %>%
  ggplot() +
  geom_bar(aes(name, words_total), stat = "identity")

"
changes in total words and unique words is also
noteworthy
"

### more used words by media
tw_words %>% 
  group_by(name) %>%
  count(word) %>%
  #arrange(name,desc(n)) %>%
  #group_by(name) %>%
  top_n(10) %>%
  mutate(word = factor(word, levels = word[order(n)])) %>%
  ggplot(aes(reorder_within(word, n, name), n, fill = name)) + #reorder_within from tidytext. reorders values but adds '_'
  geom_col() +
  scale_x_reordered() + #from tidy text. removes '_'
  #geom_bar(stat = 'identity') +
  coord_flip() +
  facet_wrap(. ~ name, scales = "free_y")

"
maybe it would be better to check proportions rather than total values.
the number of tweets is the same, but because of difference in length, it
may be better to use %?
"

### word frequencies as proportion
frequency_eco <- tw_words %>%
  filter(!name %in% c("The New York Times", "The Washington Post")) %>%
  select(name, word) %>%
  count(name, word) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  gather(name, proportion, 'Financial Times':'The Wall Street Journal')

ggplot(frequency_eco, aes(proportion, Bloomberg, color = abs(Bloomberg - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(.~name, ncol = 2) +
  theme(legend.position = "none")


frequency_pol <- tw_words %>%
  filter(name %in% c("The New York Times", "The Washington Post")) %>%
  select(name, word) %>%
  count(name, word) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  gather(name, proportion, 'The Washington Post') %>%
  mutate(`The New York Times` = `The New York Times` * 100,
         proportion = proportion * 100)

ggplot(frequency_pol, aes(proportion, `The New York Times`, color = abs(`The New York Times` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(.~name, ncol = 2) +
  theme(legend.position = "none")


frequency_total <- tw_words %>%
  select(name, word) %>%
  count(name, word) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  select(-Bloomberg) %>%
  gather(name_eco, proportion_eco, c('Financial Times','The Wall Street Journal')) %>%
  gather(name_pol, proportion_pol, c('The New York Times', 'The Washington Post')) %>%
  mutate(proportion_eco = proportion_eco * 100,
         proportion_pol = proportion_pol * 100)

ggplot(frequency_total, aes(proportion_eco, proportion_pol, color = abs(proportion_eco - proportion_pol))) +
  geom_abline(color = 'gray40', lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_gradient(limits = c(0, 1), #check this scale
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(name_eco~name_pol)
  
### correlation tests - there is a way to do a table of correlation with all media
# Financial Times ~ Bloomberg
cor.test(data = frequency_eco[frequency_eco$name == "Financial Times",], #without data doesn't work
         ~ proportion + Bloomberg)

# The Wall Street Journal ~ Bloomberg
cor.test(data = frequency_eco[frequency_eco$name == "The Wall Street Journal",],
         ~ proportion + Bloomberg)

# Financial Times ~ The Wall Street Journal
cor.test(data = frequency_eco %>% spread(name, proportion),
         ~ `Financial Times` + `The Wall Street Journal`)

# The New York Times ~ The Washington Post
cor.test(data = frequency_pol[frequency_pol$name == "The Washington Post",],
         ~ proportion + `The New York Times`)

# Financial Times ~ The New York Times
cor.test(data = frequency_total[frequency_total$name_eco == "Financial Times" & frequency_total$name_pol == "The New York Times",],
         ~ proportion_eco + proportion_pol)

# Financial Times ~ The Washington Post
cor.test(data = frequency_total[frequency_total$name_eco == "Financial Times" & frequency_total$name_pol == "The Washington Post",],
         ~ proportion_eco + proportion_pol)

# The Wall Street Journal ~ The New York Times
cor.test(data = frequency_total[frequency_total$name_eco == "The Wall Street Journal" & frequency_total$name_pol == "The New York Times",],
         ~ proportion_eco + proportion_pol)

# The Wall Street Journal ~ The Washington Post
cor.test(data = frequency_total[frequency_total$name_eco == "The Wall Street Journal" & frequency_total$name_pol == "The Washington Post",],
         ~ proportion_eco + proportion_pol)
"
at the end, it would be better to use only four outlets:
- FT and WSJ for economics
- NYT and TWP for politics
correlation does show a correspondence between the pairs. However, there is a large
correlation between WSJ and NYT, which could be explained by a specific event?
it's something that could depend on the time frame. I would probably have to limit 
the time frame for this part.
"

### sentiment analysis
#install.packages("textdata") #this is needed to access 'afinn' and 'nrc'
get_sentiments('nrc') #from tidytext




?get_sentiments

"
https://miguelgfierro.com/blog/2017/a-gentle-introduction-to-text-classification-and-sentiment-analysis/#
"
