
################  Recommendation System #############################################

#   Buisness Objective
#   Data Understanding
#   1.  Data Preperation
#   2.  Data Exploration
#   3: Recommendation Models

#Loading Neccessary libraries
library(dplyr)
library(ggplot2)
library(recommenderlab)

beer_df <- read.csv("beer_data.csv", stringsAsFactors = F, header = T)

################  Buisness Objective #############################################

# Build a recommendation system (collaborative) for your store, 
# where customers will be recommended the beer that they are most likely to buy.

################  Data Understanding: #############################################

str(beer_df)
head(beer_df)
##  The dataset contains 3 columns
#   Beer id of the beer reviewed
#   User who has rated
#   Rating

summary(beer_df)

length(unique(beer_df$beer_beerid))
# 40308 unique beers

length(unique(beer_df$review_profilename))
# 22498 unique users

###
### a >> Ratings should lie between 1-5 
###

# Min rating is coming as 0. Rating can range from 1 to 5
# Remove records with rating less than 1
beer_df <- beer_df %>%
 filter(review_overall >= 1)

summary(beer_df$review_overall)
# The range is now correct

###
### b >> Checking missing values
###

# NA
sapply(beer_df, function(x) sum(is.na(x)))
# No NA values

# Blank data
sapply(beer_df, function(y) length(which(as.character(y) == "")))
# 100 customer names as blank

# Remove blank data in review_profilename = ""
beer_df <- beer_df %>%
  filter(review_profilename != "")

###
### c >> Check for duplication
###

## Absolute duplicate rows
length(which(duplicated(beer_df[,c(1,2,3)]) == T))
# 580 absolute duplicates

beer_dup <- !duplicated(beer_df[,c(1,2,3)])
beer_df <- beer_df[beer_dup,]

## Same users rating the same beer more than once
length(which(duplicated(beer_df[,c(1,2)]) == T))
# 842 users have rated the same beer more than once

# Lets calculate the average rating given by duplicate users
beer_df <- beer_df %>%
  group_by(beer_beerid, review_profilename) %>%
  mutate(review_overall = round(mean(review_overall),1)) %>%
  distinct(review_profilename, beer_beerid, review_overall)

unique(beer_df$review_overall)
# Lets round the review to the closest 0.5

beer_df$review_overall <- sapply(beer_df$review_overall, 
                  FUN = function(x){ if((x-trunc(x)) > 0.5 ) { x= ceiling(x)}
                    else if ((x-trunc(x)) < 0.5 ) { x= floor(x)}
                    else x})

###
### d >> Lets check if atleast a minimum number of users have rated the beer
###

beer_df <- beer_df %>%
  group_by(review_profilename) %>%
  mutate(cnt_user_reviews = n()) 

summary(beer_df$cnt_user_reviews)
# There are atleast 25 percentile users who have rated 44 beers

ggplot(subset(beer_df,beer_df$cnt_user_reviews < 40), aes(x=cnt_user_reviews)) +
  geom_histogram(bins = 30, fill="tomato3") +
  scale_x_continuous(breaks = c(10,20,30,40)) +
  labs(title = "Number of reviews given to a beer for less than 40 reviews", x= "Number of reviews")

# Lets remove those users who have given less than 5 reviews to a beer for creating a 
# better recommendation model
beer_df <- beer_df %>%
  filter(cnt_user_reviews > 5)

###
unique(beer_df$review_overall)
#beer_dup <- !duplicated(beer_df[,c(1,2)])
#beer_df <- beer_df[beer_dup,]

length(unique(beer_df$beer_beerid))
# 39448 unique beers

length(unique(beer_df$review_profilename))
# 5720 users

range(beer_df$review_overall)
unique(beer_df$review_overall)
# Range of reviews is correct between 1-5
###

################  1.  Data Preperation: #############################################

## Aggregate based on reviews per beer
beer_df_cnt <- beer_df %>%
  group_by(beer_beerid) %>%
  mutate(cnt_beer_reviews = n()) %>%
  arrange(desc(cnt_beer_reviews))

summary(beer_df_cnt$cnt_beer_reviews)

ggplot(beer_df_cnt, aes(x= "Beers", y=cnt_beer_reviews)) +
  geom_boxplot(fill = "light blue", outlier.colour = "red") +
  labs(title = "Range of Number of reviews for beers", x = "Beers", y = "Number of Reviews") +
  theme(axis.text = element_text(face="bold"))

summary(beer_df_cnt$cnt_beer_reviews)
# Min 1 Review and max 977 reviews given to a beer
# Median is 89 reviws per beer

# Lets keep the minimum reviews per beer as between 1st quartile and Median as 50
beer_df_cnt <- beer_df_cnt %>%
  filter(cnt_beer_reviews >= 50)

# Lets analyse the number of reviews given to each beer
ggplot(beer_df_cnt, aes(x=cnt_beer_reviews)) +
  geom_histogram(binwidth = 50, fill = "tomato3") +
  labs(title = "Range of Number of reviews for beers", 
       x = "Number of Reviews per beer", y = "Total Number of Reviews") +
  theme(axis.text = element_text(face="bold"))

summary(beer_df_cnt$cnt_beer_reviews)
# Min 50 Review

## Lets look at number of reviews per user
## Aggregate based on reviews per user
user_df_cnt <- beer_df %>%
  group_by(review_profilename) %>%
  mutate(cnt_user_reviews = n()) %>%
  arrange(desc(cnt_user_reviews))

head(user_df_cnt)
summary(user_df_cnt$cnt_user_reviews)
# Min 1 Review and max 1842 reviews given by a user
# Median is 130 reviws per user

ggplot(user_df_cnt, aes(x=cnt_user_reviews)) +
  geom_histogram(binwidth = 50, fill = "tomato3") +
  labs(title = "Range of Number of reviews for beers", 
       x = "Number of Reviews per beer", y = "Total Number of Reviews") +
  theme(axis.text = element_text(face="bold"))

# Lets keep the minimum reviews per user as 10
user_df_cnt <- user_df_cnt %>%
  filter(cnt_user_reviews >= 10)

summary(user_df_cnt$cnt_user_reviews)
summary(user_df_cnt$review_overall)

beer_df_final <- merge(beer_df_cnt,user_df_cnt,
                       by.x=c("beer_beerid","review_profilename","review_overall"),
                       by.y=c("beer_beerid","review_profilename","review_overall"),
                       all=FALSE)

# Now we have a balanced and informed dataset to create a recommendation system
# Minimum 50 reviews given to a beer
# Users with minimum 10 reviews given

beer_df_recommender <- as.data.frame(beer_df_final[,c(2,1,3)])
beer_df_recommender$beer_beerid <- as.factor(beer_df_recommender$beer_beerid)
beer_df_recommender$review_profilename <- as.factor(beer_df_recommender$review_profilename)
beer_df_recommender$review_overall <- as.numeric(beer_df_recommender$review_overall)

beer_RRM <- as(beer_df_recommender, "realRatingMatrix")

################ 2.  Data Exploration: #############################################

## 1: How similar are the first 10 users are with each other
similar_users <- similarity(beer_RRM[1:10,],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")

# users 100floods and 0tt0 are similar

## 2: How similar are the first 10 beer are with each other
similar_items <- similarity(beer_RRM[,1:10 ],
                            method = "cosine",
                            which = "items")
as.matrix(similar_items)

image(as.matrix(similar_items), main = "Item similarity")

# Beer id 14 and 7 are similar

## 3: Unique ratings
unique(getRatings(beer_RRM))

## Visualize
# 1: The average beer ratings
avg_beer_ratings <- beer_df_final %>%
  group_by(beer_beerid) %>%
  mutate(avg_rating = mean(review_overall)) %>%
  distinct(beer_beerid, avg_rating)

ggplot(avg_beer_ratings, aes(x=avg_rating)) +
  geom_histogram(bins = 10, fill="tomato2") +
  labs(title = "The average beer ratings", 
       x = "Average Rating", y = "Number of Ratings") +
  theme(axis.text = element_text(face="bold"))  

summary(avg_beer_ratings$avg_rating)
# As evident from plot the ratings are skewed towards right with mean 3.8
# The same can be achieved using summary(colMeans(beer_RRM))

# 2: The average user ratings
avg_user_ratings <- beer_df_final %>%
  group_by(review_profilename) %>%
  mutate(avg_rating = mean(review_overall)) %>%
  distinct(review_profilename, avg_rating)

ggplot(avg_user_ratings, aes(x=avg_rating)) +
  geom_histogram(bins = 30, fill="blue3") +
  labs(title = "The average user ratings", 
       x = "Average Rating", y = "Number of Ratings") +
  theme(axis.text = element_text(face="bold")) 

summary(avg_user_ratings$avg_rating)
# The Mean and median are very close to each other. The plot can be said a lightly left skewed
# The same can be achieved using summary(rowMeans(beer_RRM))
summary(rowMeans(beer_RRM))

# 3: The average number of ratings given to the beers
avg_no_beer_rating <- beer_df_final %>%
  group_by(beer_beerid) %>%
  mutate(cnt_ratings = n()) %>%
  mutate(avg_no_of_ratings = mean(cnt_ratings)) %>%
  distinct(beer_beerid, cnt_ratings,avg_no_of_ratings)

ggplot(avg_no_beer_rating, aes(x=cnt_ratings)) +
  geom_histogram(bins = 50, fill="tomato2") +
  labs(title = "The average number of ratings given to the beers", 
       x = "Average number of Rating", y = "Number of Ratings") +
  theme(axis.text = element_text(face="bold")) 

# The average number of ratings given to a beer is highly skewed towards left
# Most of the beers getting less than 200 reviews

# 4: The average number of ratings given by the users
avg_no_user_rating <- beer_df_final %>%
  group_by(review_profilename) %>%
  mutate(cnt_ratings = n()) %>%
  mutate(avg_no_of_ratings = mean(cnt_ratings)) %>%
  distinct(beer_beerid, cnt_ratings,avg_no_of_ratings)

ggplot(avg_no_user_rating, aes(x=cnt_ratings)) +
  geom_histogram(bins = 50, fill="blue3") +
  labs(title = "The average number of ratings given to the beers", 
       x = "Average number of Rating", y = "Number of Ratings") +
  theme(axis.text = element_text(face="bold")) 

# The average number of ratings given to a beer is also highly skewed towards left
# Most of the users have reviwed less than 200 beers

################ 3: Recommendation Models: #############################################

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models

## Define parameters for IBCF and UBCF models
algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

### Experiment with 'split' and 'cross-validation' evaluation schemes

## 1: Split method
scheme_split <- evaluationScheme(beer_RRM, method = "split", train = .9,
                           k = 1, given = -1, goodRating = 4)
scheme_split

results.split <- evaluate(scheme_split, algorithms, n=c(1, 3, 5, 10, 15, 20))

plot(results.split, annotate = 1:4, legend="topleft")

# Its evident from the ROC curve that UBCF is a better choice

# Evaluate the model on test data
getData(scheme_split, "unknown")

# recommend on train data
recom_UBCF <- Recommender(getData(scheme_split, "train"), "UBCF")

recom_IBCF <- Recommender(getData(scheme_split, "train"), "IBCF")

# predict for known part
pred_UBCF_known <- predict(recom_UBCF, getData(scheme_split, "known"), type="ratings")
pred_UBCF_known

pred_IBCF_known <- predict(recom_IBCF, getData(scheme_split, "known"), type="ratings")
pred_IBCF_known

# Calculate the confusion matrix
error_split <- 
  rbind(UBCF = calcPredictionAccuracy(pred_UBCF_known, getData(scheme_split, "unknown")),
        IBCF = calcPredictionAccuracy(pred_IBCF_known, getData(scheme_split, "unknown"))
       )
error_split

# user-based collaborative filtering produces a smaller prediction error

##  2: Cross-Validation method
scheme.cv <- evaluationScheme(beer_RRM, method = "cross-validation",
                              k = 5, given = -1, goodRating = 4)
scheme.cv

results.cv <- evaluate(scheme.cv, algorithms, n=c(1, 3, 5, 10, 15, 20))

plot(results.cv, annotate = 1:4, legend="topleft")
# Its evident from the ROC curve that UBCF is a better choice

avg(results.cv)
# In both the cases the TPR increases for UBCF

## Comparing ROC curves for both methods
plot(results.split, annotate = 1:4, legend="topleft")
plot(results.cv, annotate = 1:4, legend="topleft")

## From testin both the model methods it is clear that UBCF is better option 
## for creating a recommendation model

###
### Finding the recommendations for 5 beers for users "cokes", "genog", "giblet"
###

# Creating a recommendation model using UBCF
beer_recommender <- Recommender(beer_RRM, method = "UBCF")
beer_recommender

## User cokes
beers_for_cokes <-  predict(beer_recommender, beer_RRM["cokes",], n=5)

as(beers_for_cokes, "list")
# "7971"  "34420" "17112" "1339"  "21100"

## User genog
beers_for_genog <- predict(beer_recommender, beer_RRM['genog'], n=5)

as(beers_for_genog, "list")
# "1444"  "1372"  "34420" "7971"  "1093" 

## User giblet
beers_for_giblet <- predict(beer_recommender, beer_RRM['giblet'], n=5)

as(beers_for_giblet, "list")
# "731"   "1545"  "459"   "34420" "141" 
