library(rtweet)
library(tidyverse)
library(tidytext)
library(stopwords)
library(scales) 
library(lubridate) 
library(corrplot)

theme_set(theme_light())
color1 <- c("#EC7063", "#5DADE2", "#58D68D", "#AF7AC5")


### Importing tweets
# api keys
# create_token(app = 'tw_recent',
#              consumer_key = "XXX",
#              consumer_secret = "XXX",
#              access_token = "XXX",
#              access_secret = "XXX")
# 
# tw_retrieve <- get_timeline(c("WSJ","nytimes","business","FinancialTimes","washingtonpost"), n=3200)
# 
# write_as_csv(tw_retrieve, "media.csv")

tw_original <- read_twitter_csv("media.csv")

### Overview
tw_original %>%
  group_by(name) %>%
  summarize(n = n())

# Filtering tweets and variables 
tw_media <- tw_original %>%
  mutate(date = as_datetime(created_at)) %>%
  filter(lang == "en") %>%
  select(name, date, text, source, is_retweet, favorite_count, 
         retweet_count, lang, geo_coords, status_id)

# Defining time period
tw_media %>%
  group_by(name) %>%
  mutate(n = n(),
         initial = min(date),
         diff = max(date) - min(date)) %>%
  select(name, n, initial, diff) %>% 
  unique()

tw_media <- tw_media %>%
  filter(name != 'Bloomberg',
         date >= min(date[which(name == "The New York Times")]))

# Number of tweets for each outlet
tw_media %>%
  group_by(name) %>%
  summarise(n = n())

### Word frequency
# checking for retweets
tw_media %>%
  group_by(name, is_retweet) %>%
  count()

# data cleaning
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d])|'s|(?<=\\s)')" #to filter words

tw_words <- tw_media %>%
  #filter(is_retweet == FALSE) %>% 
  select(-geo_coords, -favorite_count, -retweet_count) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
  unnest_tokens(words, text, token = "regex", pattern = reg) %>%
  filter(!words %in% stop_words$word, 
         !str_detect(words, "^000"),
         str_detect(words, "[a-z]"))

# 10 most common words
tw_words %>%
  count(words) %>%
  arrange(desc(n)) %>%
  top_n(10, n) %>%
  ggplot(aes(reorder(words,n), n)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6) + 
  ggtitle("Ten most common words") +
  xlab("Words") +
  ylab("n")

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
  scale_fill_manual(values = color1) +
  xlab('') +
  ylab('Word count')

# most used words by media
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
  scale_fill_manual(values = color1) +
  ylab('Word count') +
  xlab('Words')

### word frequencies as proportion
# Business-focused
frequency_eco <- tw_words %>%
  filter(!name %in% c("The New York Times", "The Washington Post")) %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion)

frequency_eco %>%
  filter(`The Wall Street Journal` > 0.00025,
         `Financial Times` > 0.00025) %>%
  ggplot(aes(`The Wall Street Journal`, `Financial Times`, color = abs(`Financial Times` - `The Wall Street Journal`))) +
  geom_abline(color = "black") +
  geom_point(color = "#D35400", alpha = 0.25, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(low = "#17202A", high = "#5D6D7E") +
  ggtitle("Word frequencies - Business")

# Politics-focused
frequency_pol <- tw_words %>%
  filter(name %in% c("The New York Times", "The Washington Post")) %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion)

frequency_pol %>%
  filter(`The Washington Post` > 0.00025,
         `The New York Times` > 0.00025) %>%
  ggplot(aes(`The Washington Post`, `The New York Times`, color = abs(`The New York Times` - `The Washington Post`))) +
  geom_abline(color = "black") +
  geom_point(color = "#D35400", alpha = 0.25, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(low = "#17202A", high = "#5D6D7E") +
  ggtitle("Word frequencies - Politics")

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
  filter(proportion_eco > 0.00025,
         proportion_pol > 0.00025) %>%
  ggplot(aes(proportion_eco, proportion_pol, color = abs(proportion_eco - proportion_pol))) +
  geom_abline(color = "black") +
  geom_point(color = "#D35400", alpha = 0.25, size = 3, position = position_jitter(seed = 1), show.legend = FALSE) +
  geom_text(data = subset(frequency_total, proportion_eco >= 0.0005 & proportion_pol >= 0.0005), 
            aes(label = words), check_overlap = TRUE, position = position_jitter(seed = 1), vjust = 1.5, show.legend = FALSE) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(low = "#17202A", high = "#5D6D7E") +
  ggtitle("Word frequencies - Business & Politics") +
  xlab("Business-focused") +
  ylab("Politics-focused") +
  facet_grid(name_eco ~ name_pol)

# word correlation
corr_media <- tw_words %>%
  count(name, words) %>%
  group_by(name) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(name, proportion) %>%
  drop_na() %>%
  select(-words) %>%
  cor()

corrplot(corr_media, method = "number", type = "upper", 
         col = colorRampPalette(c("black", "black", "black", "white", "darkgreen"))(20), cl.lim = c(0.5, 1)) 

# frequency and timeline
tw_media %>%
  ggplot(aes(as.Date(date), fill = name)) +  
  geom_histogram(position = 'identity', binwidth = 1, show.legend = FALSE, alpha = 0.65) +
  geom_histogram(data = subset(tw_media %>%
                                 mutate(wknds = wday(date)), 
                               wknds==7 | wknds==1), # Saturday and Sunday
                 binwidth = 1, alpha = 0.99, show.legend = FALSE) +
  ggtitle("Frequency of tweets by day") +
  xlab("Date") +
  ylab("Count") +
  scale_fill_manual(values = color1) +
  facet_wrap(.~ name, ncol = 1)

# comparison
tw_media %>%
  mutate(date = as_date(date)) %>%
  group_by(name, date) %>%
  mutate(n = n()) %>%
  ggplot(aes(date, n, color = name)) +
  geom_line(size = 1) +
  ggtitle("Frequency of tweets by day") +
  xlab("Date") +
  ylab("Count") +
  scale_color_manual(values = color1) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# daytime
tw_media %>%
  count(name, hour = hour(with_tz(date, 'EST'))) %>% 
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
  scale_fill_manual(values = color1) +
  facet_wrap(. ~ name, ncol = 1)

### lenght and number of words
# length of tweet
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;|(?<=)<(.*?)(?=>)>", "")) %>% #removes url and emojis
  mutate(length = nchar(text)) %>%
  ggplot(aes(x=name, y=length, fill = name)) +
  geom_boxplot(show.legend = FALSE) +
  ggtitle("Tweet length") +
  xlab("") +
  ylab("Length")

# reactable
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;|(?<=)<(.*?)(?=>)>", "")) %>%
  mutate(length = nchar(text)) %>%
  filter(length > 150 & name == "The Washington Post") %>%
  select(date, text, length) %>%
  sample_n(20, seed = 1) %>%
  reactable()

#distribution according status_id
tw_media %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;|(?<=)<(.*?)(?=>)>", "")) %>%
  mutate(length = nchar(text),
         date_simp = date(with_tz(date, 'EST')),
         hour = hour(with_tz(date, 'EST'))) %>%
  group_by(name, date_simp, hour) %>%
  mutate(length_hour = mean(length),
         date = paste(date_simp,hour, sep = "-")) %>%
  select(name, date, date_simp, hour, length_hour) %>% unique() %>%
  ggplot(aes(x=date, y=length_hour, fill = length_hour)) +
  geom_bar(stat = 'identity') +
  ggtitle("Average length of tweets per hour") +
  xlab("Hours") +
  ylab("Average length") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  facet_wrap(.~name, ncol = 1) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
