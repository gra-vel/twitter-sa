#install.packages("textdata")
#install.packages("wordcloud")
#install.packages("wordcloud2")
#install.packages("reshape2")

library(rtweet) #for tweet import
library(tidyverse)
library(tidytext) #tokenize
library(stopwords)

library(scales) #for scales in graphs
library(reactable) #for tables
library(lubridate) #for dates
library(textdata) #to get dictionaries for sentiment analysis
library(wordcloud2) #for wordcloud
library(wordcloud) #for comparison cloud
library(reshape2) #to change shape of df

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

"
2020-10-26 19:46:01	- WP
2020-10-26 19:30:04 - FT
2020-10-26 19:37:03	- Bloom
2020-10-26 19:45:07	- NYT
2020-10-26 19:51:06	- WSJ
"

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
         favorite_count, retweet_count, lang, geo_coords, status_id)

### clean
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@])|^'|'s)" #this term erases everything, except ' when it's in the middle of a word (it's, don't)
#([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@])) #everything that is not a character, number, hashtag, @
#([^A-Za-z\\d#@']|'(?=[A-Za-z\\d#@])|'(?![A-Za-z\\d#@])) #this term would erase everything that is not a character. the two additional terms (after |) erases ' at the every position
tw_words <- tw_media %>%
  filter(is_retweet == FALSE) %>%
  #erases tweets beginning with quotes. problem is some tweets begin with some name in quotes. tweet itself is not a quote
  #maybe erase rt before?
  #filter(!str_detect(text, '^"')) %>%
  select(-geo_coords, -favorite_count, -retweet_count) %>%
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
changes in total words and unique words is also noteworthy
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
  geom_col(show.legend = FALSE) +
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
"
In our work we have decided to apply a lexicon-based approach in order to avoid the need to generate a labelled training set. 
The main disadvantage of machine learning models is their reliance on labelled data. It is extremely difficult to ensure that 
sufficient and correctly labelled data can be obtained. Besides this, the fact that a lexicon-based approach can be more easily 
understood and modified by a human is considered a significant advantage for our work. We found it easier to generate an appropriate 
lexicon than collect and label relevant corpus. Given that the data pulled from social media are created by users from all over 
the globe, there is a limitation if the algorithm can only handle English language. Consequently, sentiment analysis algorithm 
should be more easily transformable into different languages.

Focus on differences between lexicons. Maybe using examples?. Normalizing results between lexicons would be a way to see
differences more clearly?
"
#install.packages("textdata") #this is needed to access 'afinn' and 'nrc'
#install.packages("vader")
get_sentiments('nrc') %>% #from tidytext
  group_by(sentiment) %>%
  summarize(n = n())

#dataframe for anger, trust, positive and negative
nrc_anger <- get_sentiments('nrc') %>%
  filter(sentiment == "anger")

nrc_trust <- get_sentiments('nrc') %>%
  filter(sentiment == "trust")

nrc_positive <- get_sentiments('nrc') %>%
  filter(sentiment == "positive")

nrc_negative <- get_sentiments('nrc') %>%
  filter(sentiment == "negative")

tw_words %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)
  
tw_words %>%
  inner_join(nrc_trust) %>%
  count(word, sort = TRUE)

tw_words %>%
  inner_join(nrc_positive) %>%
  count(word, sort = TRUE)

tw_words %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

library(vader)
get_vader(tw_media$text[1])
#vader_result <- vader_df(tw_media$text)
#save_as_csv(vader_result, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/vader_result.csv")
#write_csv(vader_result, "C:/Users/G3/Documents/Gabriel/Profile/Projects/twitter-sa/vader_result2.csv")

#total daily sentiment for all tweets
#get_sentiments("bing")
#get_sentiments("afinn")

tw_words %>%
  mutate(date_day = as_date(created_at)) %>%
  mutate(created_at = as.factor(strftime(date_day, format = "%j"))) %>% #%add day of the year variable, V for weeks
  #mutate(hour_year = (yday(as.POSIXct(created_at)))* 24 + hour(created_at)) %>% #for hours in a year
  inner_join(get_sentiments("bing")) %>%
  count(name, index=created_at, sentiment) %>%
  spread(sentiment, n , fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(index, sentiment, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol=1)
  
"
checking the sentiment for each day of the year in the period of interest
does not yield any meaningful result. this has to do with how words are
classified.
i've also tried with hours, but that would work with breaking news probably.
the only noticeable trend is that NYT and WSJ have more negative words (around -50), than
FT and WP (around -25). 
another way to find results will be to use a different segment? 12 hour period?
or to just check by tweet, once the period for all of them is defined? in that sense, 
i would be able to check an approximate for each account. 
"

nyt <- tw_words %>%
  filter(name == "Financial Times")

afinn <- nyt %>%
  mutate(date_day = as_date(created_at)) %>%
  mutate(created_at = as.factor(strftime(date_day, format = "%j"))) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = created_at) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  nyt %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
  nyt %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  mutate(date_day = as_date(created_at)) %>%
  mutate(created_at = as.factor(strftime(date_day, format = "%j"))) %>%
  count(method, index = created_at, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

"
also consider that there is a larger number of negative words in these lexicons.
"
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)

#counting positive and negative words
bing_word_counts <- tw_words %>%
  #filter(name == 'Financial Times') %>%
  inner_join(get_sentiments('bing')) %>%
  filter(word != "trump") %>%
  group_by(name) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts %>%
  group_by(name, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  #mutate(word = reorder(word, n)) %>%
  #ggplot(aes(word, n, fill = sentiment)) +
  ggplot(aes(reorder_within(word, n, name), n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(sentiment~name, ncol = 5, nrow = 2, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

"
problem is that trump is classified as positive word. should be excluded probably.
the plot here should include 4 columns for each outlet, and 2 rows for each method.
total three plots. one for each lexicon
"

### wordcloud
#small tutorial: https://www.littlemissdata.com/blog/wordclouds
tw_words %>%
  anti_join(stop_words) %>%
  filter(name == "Bloomberg") %>%
  count(word, sort = TRUE) %>%
  top_n(1000) %>%
  wordcloud2(size = 1)

#comparing sentiment in a wordcloud
tw_words %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word != "trump") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)
  

### term frequency
tw_words_2 <- tw_media %>%
  unnest_tokens(word, text) %>%
  count(name, word, sort = TRUE)

tw_total_words <- tw_words_2 %>%
  group_by(name) %>%
  summarise(total = sum(n))
  
tw_words_2 <- left_join(tw_words_2, tw_total_words)

ggplot(tw_words_2, aes(n/total, fill=name)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(.~name, ncol=2, scales = "free_y")

### zipf's law
freq_by_rank <- tw_words_2 %>%
  group_by(name) %>%
  mutate(rank = row_number(),
         frequent = n/total)

freq_by_rank %>%
  ggplot(aes(rank, frequent, color = name)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(frequent) ~ log10(rank), data = rank_subset)

freq_by_rank %>%
  ggplot(aes(rank, frequent, color = name)) +
  geom_abline(intercept = -1.18, slope = -0.905, color = 'gray50', linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

### bind_tf_idf
tw_words_2 <- tw_words_2 %>%
  bind_tf_idf(word, name, n)

tw_words_2 %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#more stop words: wsjwhatsnow, ft, wsjopinion, financialtimes, bw, citylab

tw_words_2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(name) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(.~name, ncol = 2, scales = "free") +
  coord_flip()
"
this could be use to find stopwords, considering that here appear the words most used
by each outlet and not the other.
"

### Bigrams
tw_bigram <- tw_media %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(name, bigram)

tw_bigram %>%
  count(bigram, sort = TRUE)

bigrams_separated <- tw_bigram %>%
  separate(bigram, c('word1', 'word2'), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != 'https') %>%
  filter(word2 != 'https')

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#tf_idf
bigram_tf_idf <- bigrams_united %>%
  count(name, bigram) %>%
  bind_tf_idf(bigram, name, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  group_by(name) %>%
  top_n(n = 10) %>%
  ggplot(aes(reorder_within(bigram, n, name), n, fill = name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(.~name, scales = "free_y")
"
it doesn't really work here, because the most used bigrams per media outlet are 
the names of the same media outlets or just marketing + url
"

### Trigrams
tw_media %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         word1 != 'https',
         word2 != 'https',
         word3 != 'https') %>%
  count(word1, word2, word3, sort = TRUE)

### sentiment analysis in bigrams (context)
#using not
bigrams_separated %>%
  filter(word1 == 'not') %>%
  count(word1, word2, sort = TRUE)

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE) %>%
  ungroup()
"
apparently, there is not a big impact of 'not' before a word.
"
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()
"
the words that most changed the value of each tweet over the entire period
"
#using list of negative words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(get_sentiments('afinn'), by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * value) %>%
  #arrange(desc(word1, contribution))
  arrange(word1,desc(abs(contribution))) %>%
  group_by(word1) %>%
  top_n(10, wt = abs(contribution)) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(.~word1, ncol = 2, scales = "free")

?top_n


"
https://miguelgfierro.com/blog/2017/a-gentle-introduction-to-text-classification-and-sentiment-analysis/#
"
