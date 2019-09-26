#Jadyn Gonzalez
#Homework Set 4
#Dr. Doosti
#September 27, 2019

#####################################
#     Movie Profitability           #
#####################################
# a)
# We are using the same dataset from HW 3 
# so need to set wd and import data
setwd("~/Documents/MGSC310")
getwd()
movies = read.csv("movie_metadata.csv")

# b) code given
# removing missing values of budget and gross
movies = movies[!is.na(movies$budget),]
movies = movies[!is.na(movies$gross),]

#creating new variables and simplifying variables
movies = movies[movies$budget<4e+8,]
movies$grossM = movies$gross/1e+6
movies$budgetM = movies$budget/1e+6
movies$profitM = movies$grossM-movies$budgetM
movies$cast_total_facebook_likes000s = movies$cast_total_facebook_likes / 1000

#setting seed then sampling training and test sets
set.seed(2019)
train_indx = sample(1:nrow(movies), 0.8 * nrow(movies), replace=FALSE)
train = movies[train_indx, ]
test = movies[-train_indx, ]

# c) number of rows four our train and test sets
nrow(train)
#3103
nrow(test)
#776

# d) building a correlation matrix
nums <- sapply(movies, is.numeric) # names of numeric variables
#we can see which variables are numeric
nums

cormat <- cor(movies[,nums], use="complete.obs")
print(cormat[,"profitM"])

#grossM - strong positive corr, 0.78
#gross  - strong positive corr, 0.78
#num_user_for_revies - moderate positive corr, 0.38

# e) Plot of correlation matrix
library(corrplot)
corrplot(cormat)

# f) regressing profitM on imdb_score and cast_total_facebook...
mod1 = lm(profitM ~ imdb_score + cast_total_facebook_likes000s, data = train)
summary(mod1)
# For every unit of imdb score, profitM is expected to incerease by 13.52 units ($13.52M)
# Additionally for every unit of facebook likes (1000 likes) profit is expected to increase
# by 0.33 units ($0.33M)
# Both values have p < 0.05 so both are statistically significant
# However our adjusted R squared value is only 0.7, this means our model only captures 7% of the variance.

# g) Estimated effect of likes
# Explained above but our coefficient for that variable is 0.33117 ~ 0.33
# which means we expect to see an increase in profit of $0.33M for evey 1000 likes

# h) p-values
# p-value for imdb_score = < 2e-16
# p-value for cast_total_facebook_likes = 1.03e-08
# p-value is the probability of finding extreme values when H0 is true, both our p-values
# are very low for this set so we can reject H0 for both

# i) 
# With imdb_score having a low p-value of p < 0.05, it is likely that changes in imdb_score are related
# to changes in profit

# j) R^2 and adjusted R^2
# R^2 is a measure of how good our model fits with the data, since our R^2 is very low < 0.7
# our model is not a good fit for the data
# Adjusted R^2 is R^2 that has been adjusted for additianal predictors in our model
# it increased if the added predictor explains more of the data than expected and decreases
# when it explains less than expected

# k) F-stat
# The F-stat gives us indication of joint significance of our predictors, basically it
# tells us if our coefficients improve our model, since our F-stat > 10 we can assume that
# our model is a better fit than the constant model.

# l) Residuals
length(mod1$residuals)
hist(mod1$residuals, breaks = 40)
# Appears to be a normal distribution

# k) R^2
# well define a function with 3 parameters since we regress on two variables
r2 = function(x,y0,y1)
{
  rss =  sum(((x + y0*train$imdb_score + y1*train$cast_total_facebook_likes000s) - train$profitM)^2)
  tss = sum((train$profitM - mean(train$profitM))^2)
  return(1 - rss/tss)
}

r2(coef(mod1)[1], coef(mod1)[2], coef(mod1)[3])
#This is in line with our lm function giving r2 of 0.0782

  