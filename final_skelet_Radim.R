Sys.setlocale("LC_ALL","English")

library(keras)
library(tensorflow)
#install_keras(tensorflow = 'gpu')

library(feather)
library(slam)
library(text2vec)
library(stringr)
library(dplyr)
library(tokenizers)
library(tidyverse)
library(tm)
library(profvis)
library(MASS)
library(lubridate)
library(textcat)
library(cld2)
library(plotly)
library(text2vec)
library(tokenizers)
library(lsa)



##### Shrinking the data + preprocessing #####
data <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")

data$private_business <- NULL
data$has_gg <- NULL
data$has_skype <- NULL
data$district_id <- NULL
data$has_phone <- NULL

#change 't' & 'f' to 0s and 1s
data <-
  data %>%
    mutate(was_promoted = ifelse(was_promoted == 't', 1, 0),
           has_phone = ifelse(has_phone == 't', 1, 0),
           is_liquid = ifelse(is_liquid == 't', 1, 0))
           
#write dataset
write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


##### Feature engineering #####

df <- read_feather('C:/Users/Radim/Documents/ph_ads_payment_indicator.feather')

subset <- 0.005

#subset the data
df_subset <- df %>% mutate(rnd = runif(dim(df)[1],0,1)) %>% filter(rnd < subset)
rm(df)

data <- df_subset
rm(df_subset)
data <- 
  data %>% 
    filter(price < 8e+08 | is.na(price))

data$description <- str_replace_all(data$description, '\\s+', ' ')
data$title <- str_replace_all(data$title, '\\s+', ' ')

data <-
  data %>%
    mutate(description = ifelse(is.na(description), '', description),
           is_price_na = as.numeric(is.na(price)),
           price = ifelse(is.na(price), 0, price),
           replies_day = ifelse(is.na(replies_day), 0, replies_day),
           replies_wk = ifelse(is.na(replies_wk), 0, replies_wk),
           replies_2wk = ifelse(is.na(replies_2wk), 0, replies_2wk),
           replies_4wk = ifelse(is.na(replies_4wk), 0, replies_4wk),
           is_liquid = ifelse(is.na(is_liquid), 0, is_liquid))

data <- 
  data %>% 
    mutate(description_char_count = nchar(description),
           description_word_count = str_count(description, ' ') + 1,
           title_char_count = nchar(title),
           title_word_count = str_count(title, ' ') + 1,
           description_dot_count = str_count(description, '\\.'), 
           title_dot_count = str_count(title, '\\.'),
           description_comma_count = str_count(description, ','),
           title_comma_count = str_count(title, ','),
           description_capital_count = str_count(description, '[A-Z]'),
           title_capital_count = str_count(title, '[A-Z]'),
           month_created = month(created_at_first), 
           weekday_created = weekdays(as.POSIXct(created_at_first)),
           hour_created = hour(created_at_first))

#write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data <- 
  data %>% 
    mutate(is_weekend_created = ifelse(weekday_created %in% c('Saturday', 'Sunday'), 1, 0),
           is_working_hour = ifelse(!(weekday_created %in% c('Saturday', 'Sunday')) & (hour_created < 17 & hour_created >= 8), 1, 0))

data <- data %>% mutate(description_lang = detect_language(description),
                        title_lang = detect_language(title))

weather_month_df <- as.data.frame(matrix(nrow = 10, ncol = 1))
colnames(weather_month_df) <- 'month_created'
weather_month_df$month_created <- 1:10

weather_month_df$av_rainfall <- c(13, 7, 13, 24, 129, 287, 354, 474, 401, 182)
weather_month_df$av_rainy_days <- c(4, 3, 4, 4, 9, 16, 22, 22, 22, 17)
weather_month_df$av_sunhours_day <- c(51, 55, 61, 68, 57, 41, 34, 34, 35, 44)

data <- merge(data, weather_month_df)

#write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


#Replacing NA's
#data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data <- 
  data %>% 
    mutate(is_price_na = as.numeric(is.na(price)))

data <-
  data %>%
    mutate(price = ifelse(is.na(price), 0, price))



#write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


##### DTM #####
sample <- 0.01

# df <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')
# df_subset <- df %>% 
#   mutate(rnd = runif(dim(df)[1],0,1)) %>% 
#   filter(rnd < sample)
# remove(df)

df_subset <- data
prep_fun <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ')
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ')
  x <- str_replace_all(x, '[:punct:]', ' ' )
  x <- str_replace_all(x, '\\s+', ' ')
  return(x)
}

tok_fun <- word_tokenizer

#tokenize the subset of dataset
df_train <- itoken(df_subset$description, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = df_subset$id, 
                   progressbar = FALSE)

#create vocab
vocab <- create_vocabulary(df_train, stopwords = stopwords("en"))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.7,
                                 doc_proportion_min = 0.001)

vectorizer <- vocab_vectorizer(pruned_vocab)

t1 <- Sys.time()
dtm_train <- create_dtm(df_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

##### LSA #####

