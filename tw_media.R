library(rtweet) #for tweet import
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

library(reactable) #for tables
library(lubridate) #for dates

### api keys
create_token(app = 'tw_recent',
             consumer_key = "Pfa0ShU6CEbgBescgHn1zFSot",
             consumer_secret = "inkW4taU6nxkQPAtBV0laDLWgnK09PJVm2Vl7BFnPUhg8b7um8",
             access_token = "1264947985458266112-qlPBFaRumU7WPwViuXTf62qIAnAwpU",
             access_secret = "1MakaYhPygWVI1e9tSjKawURNFrvdFkRTXsnXY7eZbazH")

# WSJ - @WSJ, NYT - @nytimes, Bloomberg - @business, FT - @FinancialTimes, The Washington Post - @washingtonpost

### retrieve data
#tw_retrieve <- get_timeline(c("WSJ","nytimes","business","FinancialTimes","washingtonpost"), n=3200)
#tw_retrieve2 <- get_timeline_unlimited(c("WSJ","nytimes","business","FinancialTimes","washingtonpost"), n=5000)
write_as_csv(tw_retrieve, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media.csv")
write_as_csv(tw_retrieve2, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/media2.csv")

#here I could create a nice table from the statistics book
names(tw_retrieve)
tw_retrieve %>%
  group_by(screen_name) %>%
  summarize(n = n())

#display_text width - changes in length?
#favorite_count / retweet_count - check which are enabled and disabled, what's common there?
##hashtags - don't know if i needed, maybe i can check with regex
#lang - why are there other languages
tw_media <- tw_retrieve %>%
  select(name, created_at, text, source, display_text_width, is_retweet,
         favorite_count, retweet_count, lang, geo_coords)

