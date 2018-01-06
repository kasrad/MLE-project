#####Feature engineering#####
require(feather)
require(tidyverse)

df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")
sample.size <- 0.005
df <- df %>% mutate(rnd  = runif(min = 0, max = 1, n = dim(df)[1])) %>% filter(rnd < sample.size)

summary(as.POSIXct(df$created_at_first))

months(as.POSIXct(df$created_at_first))

weather_month_df <- as.data.frame(matrix(nrow = 10, ncol = 1))
colnames(weather_month_df) <- 'month'
weather_month_df$month <- c('January', 'February', 'March', 'April',
                            'May', 'June', 'July', 'August', 'September',
                            'October')

weather_month_df$av_rainfall <- c(13, 7, 13, 24, 129, 287, 354, 474, 401, 182)
weather_month_df$av_rainy_days <- c(4, 3, 4, 4, 9, 16, 22, 22, 22, 17)
weather_month_df$av_sunhours_day <- c(51, 55, 61, 68, 57, 41, 34, 34, 35, 44)

library(readr)
cities_munic <- read_csv("cities_municipalities_ph.txt")
colnames(cities_munic)[2] <- 'city_name'
city_map <- read_csv("cities.csv")



# 
# for (i in 1 : dim(cities_munic)[1]){
# cities_munic$city_name[i] <- trimws(cities_munic$city_name[i], which = c("left"))
# cities_munic$city_name[i] <- substr(cities_munic$city_name[i], start = 1, stop = regexpr('\\s', cities_munic$city_name[i])[1]-1)
# }
# 
# df <- left_join(x = df, y = city_map, by = "city_id")
# df <- left_join(x = df, y = cities_munic, by = "city_name")



