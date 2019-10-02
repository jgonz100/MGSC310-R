#Jadyn Gonzalez
#Dr. Doosti
#Problem Set 5
#October 4, 2019

################################################
#Does increasing a movie's budget ever pay out?#
################################################

# a) Working with movies again, so well set directory and import
setwd("~/Documents/MGSC310")
getwd()
movies = read.csv("movie_metadata.csv")

# b) Using given code to perform various tasks
library(tidyverse)

#Removing NaN values
movies = movies[!is.na(movies$budget),]
movies = movies[!is.na(movies$gross),]

#Removing "" and "Not Rated" ratings
movies = movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"), ]

#Removing outliers for budget
movies = movies[movies$budget<4e+8,]

#Simplifying variables
movies$grossM = movies$gross/1e+6
movies$budgetM = movies$budget/1e+6
movies$profitM = movies$grossM-movies$budgetM

#New simple rating column using fct_lump()
#Four levels of ratings, leftover are 'Other'
movies$rating_simple = fct_lump(movies$content_rating, n = 4)

#Train and test sets
set.seed(2019)
train_indx = sample(1:nrow(movies), 0.8 * nrow(movies), replace=FALSE)
train = movies[train_indx, ]
test = movies[-train_indx, ]

# c) Linear model of grossM against budget and imdb_score
mod1 = lm(grossM ~ budgetM + imdb_score, data = train)
summary(mod1)

# d)
# The coefficient for budgetM is 1.002 ~ 1. This shows a positive relationship between
# budget and gross profit. We estimate that for every million we add to budget, gross profit
# is expected to increase by one million.

# e) Model adding the square of budgetM
mod2 = lm(grossM ~ budgetM + I(budgetM^2) + imdb_score, data = train)
summary(mod2)

# f) Marginal impact of budget
library(margins)
margins(mod2, at=list(budgetM = c(25,50,75,90,100,200,300)))
# It would make sense to increase budget at levels 25, 50, and 75, since adding 1 million to budget 
# is expected to yield over 1 million in gross profit, Levela at >= 90 Million expect to see
# returns less than a million for every million in budget.

# g) cplot
cplot(mod2, x = 'budgetM', what = 'effect', scatter = TRUE)
# It appears that there is only significant marginal effect of budget on gross when movie budgets 
# are less than 100M, movies with budgets greater than 100M dont see significant effect from budget.

######################################
#Movie residuals and predicted values#
######################################

# a) linear model of gross with rating level 'R'
mod3 = lm(grossM ~ imdb_score + budgetM + I(budgetM^2) + relevel(rating_simple, ref = "R"), data = train)
summary(mod3)

# b)
# Compared to an R rated movie, a movie with a G rating is expected to
# have a gross profit of 3.325M higher.

# c) Predict values
predicted_train = predict(mod3)
predicted_test = predict(mod3, newdata = test)

# d) Residuals
residuals_train = train$grossM - predicted_train
residuals_test = test$grossM - predicted_test

# e) Plotting residuals against predicted
#Creating dataframes to use in plot
mod3_train_df = data.frame(
  resids = residuals_train,
  predicted = predicted_train
)
mod3_test_df = data.frame(
  resids = residuals_test,
  predicted = predicted_test
)

#plotting residuals against predicted values
ggplot(mod3_train_df,aes(x=predicted_train,y=resids)) + geom_point(alpha=0.5) + geom_smooth(color="red")
ggplot(mod3_test_df,aes(x=predicted_test,y=resids)) + geom_point(alpha=0.5) + geom_smooth(color="red")
# Train appears heteroskedastic while test appears homoskedastic

# f) Plotting predicted agains true
df1 = data.frame(
  predicted = predicted_train,
  true = train$grossM
)
df2 = data.frame(
  predicted = predicted_test,
  true = test$grossM
)

ggplot(df1, aes(x = predicted, y = true)) + geom_point() + geom_smooth(color = "red")
ggplot(df2, aes(x = predicted, y = log(true))) + geom_point() + geom_smooth(color = "red")

# g) RMSE
library(forecast)
accuracy(predicted_train, train$grossM)
accuracy(predicted_test, test$grossM)

RMSE = function(t,p) {
  sqrt(sum(((t - p)^2)) * (1/length(t)))
}

RMSE_train = RMSE(train$grossM, predicted_train)
RMSE_train

RMSE_test = RMSE(test$grossM, predicted_test)
RMSE_test

# While the model may not be a good fit for the data, indicated by high RMSE it does not appear
# that we are overfitting. Both our test and train sets have similar RMSE. We could try and 
# normalize the data or do log transformation to lower RMSE.



