#####dataset cleansing
require(feather)
require(tidyverse)
df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator_full.feather")

#getting rid of empty or close-to-empty columns
df$private_business <- NULL
df$has_gg <- NULL
df$has_skype <- NULL
df$district_id <- NULL


#change 't' & 'f' to 0s and 1s
df$was_promoted[df$was_promoted == 'f'] <- 0
df$was_promoted[df$was_promoted == 't'] <- 1
df$was_promoted <- as.numeric(df$was_promoted)

df$has_phone[df$has_phone == 'f'] <- 0
df$has_phone[df$has_phone == 't'] <- 1
df$has_phone <- as.numeric(df$has_phone)

df$is_liquid[df$is_liquid == 'f'] <- 0
df$is_liquid[df$is_liquid == 't'] <- 1
df$is_liquid <- as.numeric(df$is_liquid)

#write dataset
write_feather(df, 'C:/Users/Radim/Documents/ph_ads_payment_indicator.feather')

#####detecting price outliers
df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")

#check the distribution of prices
dim(df[is.na(df$price),])
df <- df[!is.na(df$price),]
plot(quantile(df$price, probs = seq(0.9998, 1, 0.0001), na.rm = T), type = 'l')

quantile(df$price, probs = seq(0.9998, 1, 0.0001), na.rm = T)

#remove the prices that are obviously way too high & keep NA prices
df <- df %>% filter(df$price < 8.000000e+08 | is.na(df$price))










