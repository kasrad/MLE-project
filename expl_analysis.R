#require libraries
require(feather)
require(tidyverse)

df <- read_feather("C:/Users/Radim/Documents/ph_ads_payment_indicator.feather")

str(df)

#remove the prices that are obviously way too high & keep NA prices
df <- df %>% filter(df$price < 8.000000e+08 | is.na(df$price))

#how many ads were promoted
sum(df$was_promoted==1)

#price distribution, xlab capped at 60k
price_plot <- ggplot(df) + geom_density(aes(x = round(df$price,3))) + scale_x_continuous(limits = c(0, 60000)) 

#peaks correspond to 30K, 35K, 40K..
ggplotly(price_plot)

#check basic correlations between the response variable and the explanatory variables
for (j in seq(1,18,1)[sapply(df,class) == 'numeric']){
  print(paste('Corr between *was promoted* and', colnames(df)[j],
              'is',cor(df[,18],df[,j],use='pairwise.complete.obs'),
              '. The fraction of NA,s is', mean(is.na(df[,j]))))
}



#add variable count, this variable shows how many adverts has the user
df_1 <- df %>% group_by(user_id) %>% mutate(count = length(user_id))

#add variable mean_was_prom, this variable shows what is the proportion of promoted ads per user
df_2 <- df_1 %>% group_by(user_id) %>% mutate(mean_was_prom = mean(was_promoted))
rm(df_1)

#create dataframe with user_ids, counts and mean_was_proms
df_3 <- df_2 %>% select(user_id, count, mean_was_prom)
rm(df_2)

sellers_mean_prom <- df_3 %>% group_by(user_id) %>% filter(row_number()==1)

#check correlation between number of ads and proportion of promoted adverts.
#spoiler: there isn't any
cor(sellers_mean_prom$count, sellers_mean_prom$mean_was_prom)

#check the density of proportion of promoted adverts.
ggplot(sellers_mean_prom) + geom_density(aes(x=mean_was_prom))

#check the counts of adverts for sellers with high mean_was_prom
df_4 <- sellers_mean_prom %>% filter(mean_was_prom > 0.95)
ggplot(df_4) + geom_density(aes(x=count))


high_prom_sellers <- sellers_mean_prom %>% filter(mean_was_prom > 0.2)

View(high_prom_sellers %>% arrange(desc(count)))


########check categories and number of promoted ads in those

#add variable count, this variable shows how many adverts has the category
df_1 <- df %>% group_by(category_id) %>% mutate(count = length(category_id))

#add variable mean_was_prom, this variable shows what is the proportion of promoted ads per category
df_2 <- df_1 %>% group_by(category_id) %>% mutate(mean_was_prom = mean(was_promoted))
rm(df_1)

#create dataframe with category_ids, counts and mean_was_proms
df_3 <- df_2 %>% select(category_id, count, mean_was_prom)
rm(df_2)

category_mean_prom <- df_3 %>% group_by(category_id) %>% filter(row_number()==1)

#check correlation between number of ads and proportion of promoted adverts.
#spoiler: there isn't any
cor(category_mean_prom$count, category_mean_prom$mean_was_prom)

#check the density of proportion of promoted adverts.
ggplot(category_mean_prom) + geom_density(aes(x=mean_was_prom))

#check the counts of adverts for categories with high mean_was_prom
df_4 <- sellers_mean_prom %>% filter(mean_was_prom > 0.95)
ggplot(df_4) + geom_density(aes(x=count))


high_prom_cats<- category_mean_prom %>% filter(mean_was_prom > 0.2)

View(high_prom_cats %>% arrange(desc(count)))

plot(as.POSIXct())


View( df %>% filter(category_id == 222))







