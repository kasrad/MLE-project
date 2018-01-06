setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Sys.setlocale("LC_ALL","English")
 
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

####### Shrinking the data + preprocessing ######
data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data$private_business <- NULL
data$has_gg <- NULL
data$has_skype <- NULL
data$district_id <- NULL

#change 't' & 'f' to 0s and 1s
data$was_promoted[data$was_promoted == 'f'] <- 0
data$was_promoted[data$was_promoted == 't'] <- 1
data$was_promoted <- as.numeric(data$was_promoted)

data$has_phone[data$has_phone == 'f'] <- 0
data$has_phone[data$has_phone == 't'] <- 1
data$has_phone <- as.numeric(data$has_phone)

data$is_liquid[data$is_liquid == 'f'] <- 0
data$is_liquid[data$is_liquid == 't'] <- 1
data$is_liquid <- as.numeric(data$is_liquid)

#write dataset
write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


######## Feature engineering #########

data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data <- data %>% filter(price < 8e+08 | is.na(price))

data$description <- str_replace_all(data$description, '\\s+', ' ')
data$title <- str_replace_all(data$title, '\\s+', ' ')

data <- data %>% mutate(description_char_count = nchar(description),
                        description_word_count = str_count(description, ' ') + 1,
                        title_char_count = nchar(title),
                        title_word_count = str_count(title, ' ') + 1,
                        description_dot_count = str_count(description, '\\.'), 
                        title_dot_count = str_count(title, '\\.'),
                        description_comma_count = str_count(description, ','),
                        title_comma_count = str_count(title, ','),
                        description_capital_count = str_count(description, '[A-Z]'),
                        title_capital_count = str_count(title, '[A-Z]'),
                        month_created = month(as.POSIXct(data$created_at_first)), 
                        weekday_created = weekdays(as.POSIXct(created_at_first)))

data <- data %>% mutate(is_weekend_created = ifelse(weekday_created %in% c('Saturday', 'Sunday'), 1, 0))
data <- data %>% mutate(description_lang = detect_language(description),
                        title_lang = detect_language(title))

weather_month_df <- as.data.frame(matrix(nrow = 10, ncol = 1))
colnames(weather_month_df) <- 'month_created'
weather_month_df$month_created <- 1:10

weather_month_df$av_rainfall <- c(13, 7, 13, 24, 129, 287, 354, 474, 401, 182)
weather_month_df$av_rainy_days <- c(4, 3, 4, 4, 9, 16, 22, 22, 22, 17)
weather_month_df$av_sunhours_day <- c(51, 55, 61, 68, 57, 41, 34, 34, 35, 44)

data <- merge(data, weather_month_df)

write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')





