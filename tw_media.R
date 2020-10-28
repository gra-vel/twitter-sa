library(rtweet) #for tweet import
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

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
  head(20)

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


?as_date
