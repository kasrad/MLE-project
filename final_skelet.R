Sys.setlocale("LC_ALL","English")
setwd("~/ML_Naspers")


# load('C:/Users/Samuel/Documents/ML_Naspers/Data/lda__doc_distr.RData')
# load('C:/Users/Samuel/Documents/ML_Naspers/Data/df_withfeatures_nodummies.RData')
# load('C:/Users/Samuel/Documents/ML_Naspers/Data/df__dtm_descr__dtm_title.RData')


library(keras)
library(tensorflow)
library(dplyr)
library(mlr)

#install_keras(tensorflow = 'gpu')

library(feather)
library(slam)
library(text2vec)
library(stringr)
library(dplyr)
library(tokenizers)
library(tidyverse)
library(tm)
library(lubridate)
library(cld2)
library(lsa)
library(dummies)
library(mlr)
library(kernlab)


##### Shrinking the data + preprocessing #####
data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data$private_business <- NULL
data$has_gg <- NULL
data$has_skype <- NULL
data$district_id <- NULL
data$has_phone <- NULL

#change 't' & 'f' to 0s and 1s
data <-
  data %>%
  mutate(was_promoted = ifelse(was_promoted == 't', 1, 0),
         is_liquid = ifelse(is_liquid == 't', 1, 0))

#write dataset
write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


##### Feature engineering #####

data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

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

write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data <- 
  data %>% 
  mutate(is_weekend_created = ifelse(weekday_created %in% c('Saturday', 'Sunday'), 1, 0),
         is_working_hour = ifelse(!(weekday_created %in% c('Saturday', 'Sunday')) & (hour_created < 17 & hour_created >= 8), 1, 0))

data <- 
  data %>% 
  mutate(description_lang = detect_language(description),
         title_lang = detect_language(title))

data <-
  data %>% 
  mutate(description_lang = ifelse(is.na(description_lang), 'uncertain', description_lang),
         title_lang = ifelse(is.na(title_lang), 'uncertain', title_lang))


weather_month_df <- as.data.frame(matrix(nrow = 10, ncol = 1))
colnames(weather_month_df) <- 'month_created'
weather_month_df$month_created <- 1:10

weather_month_df$av_rainfall <- c(13, 7, 13, 24, 129, 287, 354, 474, 401, 182)
weather_month_df$av_rainy_days <- c(4, 3, 4, 4, 9, 16, 22, 22, 22, 17)
weather_month_df$av_sunhours_day <- c(51, 55, 61, 68, 57, 41, 34, 34, 35, 44)

data <- merge(data, weather_month_df)

data <-
  data %>%
  group_by(user_id) %>%
  mutate(user_ad_count = length(user_id),
         user_mean_was_promoted = mean(was_promoted))

data <-
  data %>%
  group_by(category_id) %>%
  mutate(category_ad_count = length(category_id),
         category_mean_was_promoted = mean(was_promoted))


write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')




#Replacing NA's
data <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

data <- 
  data %>% 
  mutate(is_price_na = as.numeric(is.na(price)))

data <-
  data %>%
  mutate(price = ifelse(is.na(price), 0, price))


write_feather(data, 'C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')


##### DTM #####
df <- read_feather('C:/Users/Samuel/Documents/ML_Naspers/Data/ph_ads_payment_indicator.feather')

prep_fun <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ')
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ')
  x <- str_replace_all(x, '[:punct:]', ' ' )
  x <- str_replace_all(x, '\\s+', ' ')
  return(x)
}

tok_fun <- word_tokenizer

#description
#tokenize the subset of dataset
df_train_descr <- itoken(df$description, 
                         preprocessor = prep_fun, 
                         tokenizer = tok_fun, 
                         ids = df$id, 
                         progressbar = FALSE)

#create vocab
vocab <- create_vocabulary(df_train_descr, stopwords = stopwords("en"))
pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.7,
                                 doc_proportion_min = 0.001)

vectorizer <- vocab_vectorizer(pruned_vocab)

t1 <- Sys.time()
dtm_train <- create_dtm(df_train_descr, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))


#title
df_train_title <- itoken(df$title,
                         preprocessor=prep_fun,
                         tokenizer=tok_fun,
                         ids=df$id,
                         progressbar=FALSE)

vocab_title <- create_vocabulary(df_train_title, stopwords = stopwords('eng'))
pruned_vocab_title <- prune_vocabulary(vocab_title, 
                                       term_count_min = 10,
                                       doc_proportion_min = 0.001, 
                                       doc_proportion_max = 0.7)

vectorizer_title <- vocab_vectorizer(pruned_vocab_title)


t1 <- Sys.time()
dtm_train_title <- create_dtm(df_train_title, vectorizer_title)
print(difftime(Sys.time(), t1, units = 'sec'))


save(df, dtm_train, dtm_train_title, file = 'C:/Users/Samuel/Documents/ML_Naspers/Data/df__dtm_descr__dtm_title.RData')

##### LDA #####
#description
lda_model <- LDA$new(n_topics=50, doc_topic_prior=0.1, topic_word_prior=0.1)
doc_topic_distr <- lda_model$fit_transform(x = dtm_train, n_iter = 10000,
                                           convergence_tol = 0.001, n_check_convergence = 25,
                                           progressbar = T)


lda_model$get_top_words(n = 20, topic_number = c(1L, 5L, 10L), lambda = 1)

#title
lda_model_title <- LDA$new(n_topics=50, doc_topic_prior=0.1, topic_word_prior=0.1)
doc_topic_distr_title <- lda_model_title$fit_transform(x = dtm_train_title, n_iter = 10000,
                                                       convergence_tol = 0.001, n_check_convergence = 25,
                                                       progressbar = T)

lda_model_title$get_top_words(n = 20, topic_number = c(1L, 5L, 10L), lambda = 1)


save(lda_model, doc_topic_distr, lda_model_title, doc_topic_distr_title, file = 'C:/Users/Samuel/Documents/ML_Naspers/Data/lda__doc_distr.RData')




##### Data preparation for keras #####

df <-
  df %>%
  mutate(part_of_the_day = case_when(hour_created > 2 & hour_created <= 11 ~ 'morning',
                                     hour_created > 11 & hour_created <= 15 ~ 'noon',
                                     hour_created > 15 & hour_created <= 18 ~ 'afternoon',
                                     hour_created > 18 & hour_created <= 22 ~ 'evening',
                                     hour_created > 22 | hour_created <= 2 ~ 'night'))

df <- 
  df %>%
  mutate(description_lang = ifelse(description_lang == 'en', 'en', 'else'),
         title_lang = ifelse(title_lang == 'en', 'en', 'else'))


# Convertin categorical variables to dummy variables
categorical_columns <- c('part_of_the_day', 'month_created', 'weekday_created', 'description_lang', 'title_lang')

for (col in categorical_columns){
  
  col_pos <- match(col, colnames(df))
  dummy_matrix <- dummy(as.matrix(df[, col_pos]))
  colnames(dummy_matrix) <- paste(col, as.character(sort(as.matrix(unique(df[, col_pos])))), sep = '')
  
  df <- df[, -col_pos]
  df <- cbind(df, dummy_matrix)
  
  print(col)
  rm(col_pos, dummy_matrix)  
}


doc_topic_distr <- as.data.frame(doc_topic_distr)
doc_topic_distr_title <- as.data.frame(doc_topic_distr_title)

colnames(doc_topic_distr) <- paste('desc_topic_', 1:50, sep = '')
colnames(doc_topic_distr_title) <- paste('title_topic_', 1:50, sep = '')


if(sum(df$id == rownames(doc_topic_distr)) == nrow(df) & sum(df$id == rownames(doc_topic_distr_title)) == nrow(df)){
  df <- cbind(df, doc_topic_distr)
  rm(doc_topic_distr)
  df <- cbind(df, doc_topic_distr_title)
  rm(doc_topic_distr_title)
  
  print('Done')
}




df <-
  df %>%
  select(-c(hour_created, title, description, user_id, subregion_id, created_at_first, id))


save(df, file = 'C:/Users/Samuel/Documents/ML_Naspers/Data/df_withLDA.RData')


##### Adding categories, cities, regions #####

load('C:/Users/Samuel/Documents/ML_Naspers/Data/df_withLDA.RData')

#category
categories_grouped <- read_delim(file = 'C:/Users/Samuel/Documents/ML_Naspers/categories_grouped.csv', delim = ';')
categories_grouped <-
  categories_grouped %>%
  select(-X1)

colnames(categories_grouped)[1] <- 'category_id'
col_cat <- colnames(categories_grouped)[2:6]
colnames(categories_grouped)[2:6] <- paste('category_id', col_cat, sep = '')

category <- 
  df %>%
  select(category_id)

grouped_category <- merge(category, categories_grouped)

grouped_category <- 
  grouped_category %>%
  select(-category_id)

df <- cbind(df, grouped_category)

rm(categories_grouped, category, grouped_category, col_cat)

#cities  
cities_grouped <- read_delim(file = 'C:/Users/Samuel/Documents/ML_Naspers/cities_count.csv', delim = ';')

col_cities <- colnames(cities_grouped)[2:7]
colnames(cities_grouped)[2:7] <- paste('city_id', col_cities, sep = '')

city_id <- 
  df %>%
  select(city_id)


city_id_grouped <- merge(city_id, cities_grouped)

city_id_grouped <-
  city_id_grouped %>%
  select(-city_id)

df <- cbind(df, city_id_grouped)

rm(city_id, cities_grouped, city_id_grouped, col_cities)

#regions
regions_grouped <- read_delim(file = 'C:/Users/Samuel/Documents/ML_Naspers/regions_count.csv', delim = ';')

col_reg <- colnames(regions_grouped)[2:7]
colnames(regions_grouped)[2:7] <- paste('region_id', col_reg, sep = '')

region_id <- 
  df %>%
  select(region_id)

region_id_grouped <- merge(region_id, regions_grouped)

region_id_grouped <-
  region_id_grouped %>%
  select(-region_id)

df <- cbind(df, region_id_grouped)

rm(region_id, region_id_grouped, regions_grouped, col_reg)


df <-
  df %>%
  select(-c(city_id, region_id, category_id))

save(df, file = 'C:/Users/Samuel/Documents/ML_Naspers/Data/dataset_kerasready.RData')

##### Correlations #####

rm(list = ls())
load('C:/Users/Samuel/Documents/ML_Naspers/Data/dataset_kerasready.RData')

correlations <- as.data.frame(matrix(ncol=2, nrow=ncol(df)))
for (i in 1:ncol(df)){
  correlations[i, 1] <- colnames(df)[i]
  correlations[i, 2] <- cor(df[, i], df[ , 'was_promoted'])
  print(i)
}


##### OVERSAMPLING #####
rm(list=ls())

# load the data + sample
load('C:/Users/Samuel/Documents/ML_Naspers/Data/dataset_kerasready.RData')

sample_size <- 0.1

df <-
  df %>%
  mutate(rnd = runif(nrow(df))) %>%
  filter(rnd < sample_size) %>%
  select(-rnd)

df <-
  df %>%
  select(-was_promoted, was_promoted)

rnd = runif(nrow(df))


# partition
df_test <- df[rnd > 0.9, ]
df_train <- df[rnd <= 0.9, ]

rm(df, rnd, sample_size)

# conversion to factors for smote
binary_columns <- c(2, 7, 8, 19, 20, 28, 29, 30, 31, 32, 33, 
                    34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
                    44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 
                    154, 155, 156, 157, 158, 159, 160, 161, 
                    162, 163, 164, 165, 166, 167, 168, 169, 
                    170, 171)

binary_colnames <- as.character(lapply(binary_columns, function(x) colnames(df_train)[x]))
df_train[binary_colnames] <- lapply(df_train[binary_colnames], as.factor)

task = makeClassifTask(data = df_train, target = "was_promoted")
task.under = undersample(task, rate = 1/3)
task.over = oversample(task.under, rate = 10)

df_train <- getTaskData(task.over)

table(df_train$was_promoted)

# task = makeClassifTask(data = df_train, target = "was_promoted")
# task.smote = smote(task, rate = 11, nn = 5)
#
# df_under <- getTaskData(task.under)
# df_train <- getTaskData(task.smote)

df_train[binary_colnames] <- lapply(df_train[binary_colnames], function(x) as.numeric(as.vector(x)))

rm(task, task.under, task.smote, binary_colnames, binary_columns, task.over)


##### SVM #####
rm(list = ls())
load('~/ML_Naspers/Data/XY_train__XY_test__90_10_split.RData')

#launch empty model, first argument corresponds to n# of columns in input

y_train <- as.matrix(y_train[, 2])
y_test <- as.matrix(y_test[, 2])

for (i in 1:length(y_train)) {
  y_train[i, 1] <- ifelse(y_train[i, 1] == 0, -1, 1)
  y_test[i, 1] <- ifelse(y_test[i, 1] == 0, -1, 1)
}

table(y_train)
table(y_test)

on <- inlearn(ncol(x_train), kernel = rbfdot(sigma = 0.01), type = "classification")



#xs are inputs, ys are outputs
for (i in 1:100000) {
  on <- onlearn(on, x = x_train[i, ], y = y_train[i, 1])
  print(i)
}

table(sign(predict(on, x_test)))
# NAPICUUUUUUUUUUUU



##### ANN #####
y_train <-
  df_train %>%
  select(was_promoted) %>%
  unname() %>%
  as.matrix() %>%
  to_categorical()

x_train <-
  df_train %>%
  select(-was_promoted) %>%
  unname() %>%
  as.matrix()

rm(df_train)

y_test <-
  df_test %>%
  select(was_promoted) %>%
  unname() %>%
  as.matrix() %>%
  to_categorical()

x_test <-
  df_test %>%
  select(-was_promoted) %>%
  unname() %>%
  as.matrix()

rm(df_test)


rm(model, history)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 128, activation='sigmoid', input_shape = c(ncol(x_train)))  %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 64)  %>%
  layer_activation_leaky_relu(alpha = 0.3) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

#class_weight = list('0' = 1, '1' = 40)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(lr=0.002),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 2, batch_size = 128, 
  validation_split = 0.3,
#  class_weight = class_weight
)

model %>% evaluate(x_test, y_test)

result <- cbind(y_test, model %>% predict_classes(x_test))

confusion_table <- matrix(ncol = 2, nrow = 2)

colnames(confusion_table) <- c('TRUE', 'FALSE')
row.names(confusion_table) <- c('predTRUE', 'predFALSE')
confusion_table[1, 1] <- sum(result[,2] == 1 & result[,3] == 1)/dim(result)[1]
confusion_table[2, 1] <- sum(result[,2] == 1 & result[,3] == 0)/dim(result)[1]
confusion_table[1, 2] <- sum(result[,2] == 0 & result[,3] == 1)/dim(result)[1]
confusion_table[2, 2] <- sum(result[,2] == 0 & result[,3] == 0)/dim(result)[1]
print(confusion_table)

print(paste('The probability that customer will buy an advert if TRUE is',
            confusion_table[1,1]/sum(confusion_table[1,])))

print(paste('The probability that customer will not buy an advert if FALSE is',
            confusion_table[2,2]/sum(confusion_table[2,])))


##### FOR LOOP #####
rm(list = ls())
load('~/ML_Naspers/Data/XY_train__XY_test__90_10_split.RData')




