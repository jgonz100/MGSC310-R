# Jadyn Gonzalez
# Dr. Doosti
# MGSC 310
# October 25, 2019

#################
# Problem Set 8 #
#################

###################################
# What Predicts Bike Share Usage? #
###################################

# a) setting directory and importing day.csv 
setwd("~/Documents/MGSC310/Bike-Sharing-Dataset")
Bike_DF = read.csv("day.csv")
summary(Bike_DF)
str(Bike_DF)

# b) basic data cleaning and conversions to factors
# Taking care of the season attribute
Bike_DF$season = factor(format(Bike_DF$season, format = "%A"),
                          levels = c("1", "2","3","4"),
                          labels = c("Spring", "Summer", "Fall", "Winter"))
# Next convert holiday into a factor
Bike_DF$holiday = factor(format(Bike_DF$holiday, format = "%A"),
                           levels = c("0", "1"), 
                           labels = c("No", "Yes"))
# Convert weatersit to a factor
Bike_DF$weathersit = factor(format(Bike_DF$weathersit, format = "%A"),
                              levels = c("1", "2","3","4"), 
                              labels = c("Clear", "Cloudy", "Light Storms", "Heavy Storms"))

# Fix the yr variable
Bike_DF$yr = factor(format(Bike_DF$yr, format = "%A"),
                      levels = c("0", "1"), 
                      labels = c("2011","2012"))
# Convert workingday
Bike_DF$workingday = factor(format(Bike_DF$workingday, format = "%A"),
                    levels = c("0", "1"), 
                    labels = c("No","Yes"))


# c) sapply command
sapply(Bike_DF, is.factor) 
# looks like everything is good
str(Bike_DF)

# d) Feature Transformation
# Next we'll add in some variables as some were normalized
# temp: need to multiply by 41
# atemp: (feeling) need to multiply by 50
# hum: need to multiply by 100
# windspeed: need to multiply by 67
Bike_DF$actual_temp = Bike_DF$temp*41 # in celsius
Bike_DF$feel_temp = Bike_DF$atemp*50 # in celsius
Bike_DF$actual_windspeed = Bike_DF$windspeed*67
Bike_DF$actual_humidity = Bike_DF$hum*100 # percent
summary(Bike_DF)

# e) Train and Test 70/30 split
set.seed(310)
train_idx = sample(1:nrow(Bike_DF),size = floor(0.70*nrow(Bike_DF)))
Bike_train = Bike_DF[train_idx,]
Bike_test = Bike_DF[-train_idx,]

# f) Forward stepwise model with cnt as predicted
library(leaps)
fwd_fit = regsubsets(cnt ~ season + holiday + mnth + weathersit + workingday + temp + hum + windspeed,
                     data = Bike_train,
                     nvmax = 10,
                     method = "forward")
summary(fwd_fit)
# First five variables selected
# 1. temp
# 2. seasonWinter
# 3. hum
# 4. windspeed
# 5. seasonSummer

# g) Backwards stepwise
back_fit = regsubsets(cnt ~ season + holiday + mnth + weathersit + workingday + temp + hum + windspeed,
                     data = Bike_train,
                     nvmax = 10,
                     method = "backward")
summary(back_fit)

# Five variables in M5 are the same as the fwd_fit model
# We may not always get the same variables in stepwise selection because
# when using the forward method, we add variables one at a time, the addition of
# a new variable might cause a previous variable to become insignificant
# if we use bacward selection we drop variables that are least significant
# until we get a set of variables that are all significant

# h) Ridge model
library(ggplot2)
library(glmnet)
library(glmnetUtils)
train_subset = subset(Bike_train, select = -c(dteday,instant))
ridge_fit = cv.glmnet(cnt ~ .,
                      data = train_subset,
                      alpha = 0)
plot(ridge_fit)

# i) lambda.min and lambda
# Value of lambda.min
ridge_fit$lambda.min
# Value of lambda.1se
ridge_fit$lambda.1se
as.matrix(coef(ridge_fit, s =
                 "lambda.min")) 
# lambda.min gives lambda that produces the minumum mean cv error
# lambda.1se gives lambda where the most regularized model iw within 1se of the min

# j) coefficients
coefmat = data.frame(
  ridge_min = as.matrix(round(coef(ridge_fit, s = ridge_fit$lambda.min), 3)),
  ridge_1se = as.matrix(round(coef(ridge_fit, s = ridge_fit$lambda.1se), 3))
)

colnames(coefmat) = c("Ridge Min", "Ridge 1se")

coefmat
# All the coefficients increase when lambda increases 
# except for casual and registed, they decrease with a higher lambda
# which is interesting as one would think they would go up as
# they should have high correlation between count.

# k) Lasso model
lasso_fit = cv.glmnet(cnt ~ .,
                      data = train_subset,
                      alpha = 1)

# l) coefficients
lasso_fit$lambda.min
lasso_fit$lambda.1se

coefmat = data.frame(
  lasso_min = as.matrix(round(coef(lasso_fit, s = lasso_fit$lambda.min), 3)),
  lasso_1se = as.matrix(round(coef(lasso_fit, s = lasso_fit$lambda.1se), 3))
)

colnames(coefmat) = c("Lasso Min", "Lasso_1se")

coefmat
# Lasso chose two variables registered and casual

# m) 
# While lasso retained the most significant variables in the model, count of registered users, and
# count of casual users, it completely dropped (set to 0) any other variables that may have 
# been affecting bike share usage. In this case I would chose the ridge model over the lasso because
# the ridge model gives us more variables that have an impact on the number of bike shares. The variables
# from the lasso do not gives us any interesting insgight on the data, it is obvious that the number of 
# users affects the number of bike shares.





