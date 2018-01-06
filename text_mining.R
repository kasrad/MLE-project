#require libraries
require(feather)
require(tidyverse)
require(stringr)
require(plotly)
require(text2vec)
require(tokenizers)
require(slam)

set.seed(1211)

#read data
df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")

#how many items in each category
length(table(df$category_id))

#subset
subset <- 0.005

#subset the data
df_subset <- df %>% mutate(rnd = runif(dim(df)[1],0,1)) %>% filter(rnd < subset)
rm(df)


#define preparation and tokenize functions
prep_fun1 = function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ')
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ')
  x <- str_replace_all(x, '[:punct:]', ' ' )
  x <- str_replace_all(x, '\\s+', ' ')
  return(x)
}

tok_fun = word_tokenizer

#tokenize the subset of dataset
df_train = itoken(df_subset$description, 
                  preprocessor = prep_fun1, 
                  tokenizer = tok_fun, 
                  ids = df_subset$id, 
                  progressbar = FALSE)

#create vocab
vocab = create_vocabulary(df_train, stopwords = stopwords(language = c("en")))
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.9,
                                doc_proportion_min = 0.0005)

vectorizer = vocab_vectorizer(pruned_vocab)

#create dtm matrix
t1 = Sys.time()
dtm_train = create_dtm(df_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

dim(dtm_train)

#inspect dtm matrix
summary(col_sums(dtm_train))
sort(col_sums(dtm_train), decreasing = TRUE)[1:10]
sum(col_sums(dtm_train) > 2000) #more efficient than the line below
#length(col_sums(dtm_train)[col_sums(dtm_train)>2000])


#rm docs with zero words
dim(dtm_train)
dtm_train <- dtm_train[row_sums(dtm_train)>0, ]
dim(dtm_train)

dtm_train_help <- dtm_train %>% mutate(sums = rowSums(dtm_train))

tf <- dtm_train_help %>% mutate_all(funs(. / sums))
rm(dtm_train_help)

tf <- tf %>% select(-sums)

tf_vec <- sapply(tf,mean)

#idf
idf <- log2(dim(dtm_train)[2]/col_sums(dtm_train))

#tfidf statistic
term_tfidf <- tf_vec * idf

summary(term_tfidf)

#remove rows with low tfidf
dtm_new <- dtm_train[,term_tfidf >= 0.0008]

#inspect final dtm matrix
summary(col_sums(dtm_new))

#visualize the most frequent words
library(wordcloud)
freq = data.frame(freqterms = sort(col_sums(dtm_new), decreasing = TRUE))

wordcloud(rownames(freq), freq[,1], max.words = 50, colors = brewer.pal(3, "Dark2"))

