# Jadyn Gonzalez
# Dr. Doosti
# MGSC 310
# October 18, 2019

#################
# Problem Set 7 #
#################

setwd("~/Documents/MGSC310")
movies = read.csv("movie_metadata.csv")

########################################
# 1: What Predicts Blockbuster Movies? #
########################################

# a) Running the given code to clean the data and generate train and test sets
options(scipen = 50)
# removing missing values
movies = movies[complete.cases(movies),]

# removing empty content rating or not rated
movies = movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"), ]

# removing movies with budget > 400M
movies = movies[movies$budget < 400000000,]

# creating budget, gross, and profit columns in millions
movies$grossM = movies$gross/1e+6
movies$budgetM = movies$budget/1e+6
movies$profitM = movies$grossM - movies$budgetM

# creating a column for main genre
movies$genre_main = do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]

# creating a dummy for blockbuster movies
movies$blockbuster = ifelse(movies$grossM > 200, 1, 0)
library(forcats)
movies$genre_main = fct_lump(movies$genre_main,5)
movies$content_rating = fct_lump(movies$content_rating,3)
movies$country = fct_lump(movies$country,2)
movies$cast_total_facebook_likes000s = movies$cast_total_facebook_likes / 1000

# top director
director_props = data.frame(prop.table(table(movies$director_name)))
directors_indx = order(director_props$Freq,decreasing = TRUE)
top_directors_indx = directors_indx[1:floor(0.1*nrow(director_props))]
top_directors_names = director_props[top_directors_indx, 1]
movies$top_director = ifelse(movies$director_name %in% top_directors_names, 1, 0)

# train/test split
set.seed(1861)
train_idx = sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train = movies[train_idx,]
movies_test = movies[-train_idx,]

# b) Calculating mean for blockbuster in the test/train sets
mean(movies_train$blockbuster)
mean(movies_test$blockbuster)
t.test(movies_train$blockbuster, movies_test$blockbuster,paired = TRUE)

# c) Log model of blockbuster against several variables
mod1 = glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s
           + content_rating + genre_main, data = movies_train, family = binomial)
summary(mod1)
exp(mod1$coefficients)

# d) Interpret content_ratingR, genre_mainAdventure and TopDirector
# The p value of content_ratingR means that it is statistically significant so, when we take the
# exponent of the coefficient we get 0.147. This means that compared to a G rated movie (since
# content rating G is not present) a movie with a rating of R is expected to be (.147-1) 85.3% 
# less likely to be a blockbuster.

# Lets list the genres before we interpret Adventure
summary(movies$genre_main)
# We see that Action movies are the reference variable in our model. The p value is high for 
# Adventure movies so it is statistically insignificant. If it was the exponent of the coefficient
# is 1.54 which means that compared to Action movies, Adventure movies are expected to be
# (1.54-1) 54% more likely to be a blockbuster.

# The p value for top_director tells us that the variable is statistically significant. When we 
# take the exponent of the coefficient we get 1.81 which means that having a top director
# increases the probability that a movie is a blockbuster by (1.81-1) 81%.

# e) Generate predictions for train and test
predsTrain = data.frame(
  movies_train,
  scores = predict(mod1, type = "response")
)

predsTest = data.frame(
  movies_test,
  scores = predict(glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s
                       + content_rating + genre_main, data = movies_test, family = binomial), type = "response")
)

# f) LOOCV for train
preds_LOOCV = NULL
for(i in 1:nrow(movies_train)){
  mod = glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s
            + content_rating + genre_main, data = movies_train[-i,], family = binomial)
  preds_LOOCV[i] = predict(mod, newdata = movies_train[i,])
}

# g) Plotting ROC curves
library(plotROC)
TrainROC = ggplot(predsTrain, aes(m = scores, d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.99, 0.9, 0.7, 0.6, 0.5, 0.4, 0.1, 0.01))
print(TrainROC)

# It looks like the confidence value of 0.1 would generate the highest AUC
# meaning that our model has relatively high predicting power.

TestROC = ggplot(predsTest, aes(m = scores, d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.99, 0.9, 0.7, 0.6, 0.5, 0.4, 0.1, 0.01))
print(TestROC)

# This curve is similar to the train set and predictions and has similar interpretation. However 
# the curve is more rigid and appears to have a smaller AUC, this is expected and is likely
# due to sampling variation and overall less samples.

LOOCVDF = data.frame(
  movies_train,
  scores = preds_LOOCV
)
LOOROC = ggplot(LOOCVDF, aes(m = scores, d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.99, 0.9, 0.7, 0.6, 0.5, 0.4, 0.1, 0.01))
print(LOOROC)

# This curve also looks similar to the others but it appears that using LOOCV
# has smoothed our curve. Which means it likely cut reduced the variance we were getting
# in our test set

# h) AUC from least to greatest
LOOCVAUC = calc_auc(LOOROC)
print(LOOCVAUC)

TestAUC = calc_auc(TestROC)
print(TestAUC)

TrainAUC = calc_auc(TrainROC)
print(TrainAUC)

# In general we expect to see better in sample performance which explains the higher value
# of our train AUC. Test AUC was higher than LOOCV likely due to increased variance in the set
# which was reduced by using LOOCV.

