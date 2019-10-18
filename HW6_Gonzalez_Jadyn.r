# Jadyn Gonzalez
# Dr. Doosti.
# MGSC 310
# Problem Set 6
# October 10, 2019


###############################
# Predicting Expensive Houses #
###############################

# a) Using the given code to generate test and train sets for the 'Boston' data
library(MASS)
data(Boston)
options(scipen = 999)
#?Boston
# a binary outcome for pricey home
Boston$PriceyHome <- ifelse(Boston$medv > 40, 1, 0)
# converting chas into a factor
Boston$chas <- factor(Boston$chas)
set.seed(2019)
trainSize <- 0.75
train_idx <- sample(1:nrow(Boston), size = floor(nrow(Boston) * trainSize))
housing_train <- Boston[train_idx,]
housing_test <- Boston[-train_idx,]

# b) Average neighborhood differences over pricey v non pricey homes
library(doBy)
summaryBy(. ~ PriceyHome, data = housing_train)
# We see the greates differences in zoned land, Pricey homes have on average double the land
# tax rate, non Pricey homes have much higher mean tax rate
# lstat, non Pricey homes have an average three times lower population status than Pricey homes
# medv, Pricey homes have more than double the median value on average
# indus, non pricey homes have a greater average proportion of non-retail business

# c) Plotting these differences
library(ggplot2)
ggplot(data = housing_train, aes(x = tax, y = lstat)) + 
  geom_point(aes(color = PriceyHome))
ggplot(data = housing_train, aes(x = tax, y = medv)) + 
  geom_point(aes(color = PriceyHome))
ggplot(data = housing_train, aes(x = zn, y = lstat)) + 
  geom_point(aes(color = PriceyHome))
ggplot(data = housing_train, aes(x = medv, y = lstat)) + 
  geom_point(aes(color = PriceyHome))

# There are less pricey homes compared to non pricey homes
# In general, pricey homes have higher median value and tax rates
# non pricey homes have higher lstat and zoned land
# higher lstat value means we expect to see lower medv 
# higher medv we expect to see lower lstat

# d) Logistic model of pricey home against chas
# These values are already binary (0,1) so we dont need to change them
mod1 = glm(PriceyHome ~ chas, family = binomial, data = housing_train)
summary(mod1)
exp(mod1$coefficients)
# Being adjacent to the Charles river affects the probability that a home is 
# a pricey home by an order of 5.34

# e) Same model with more variables
mod2 = glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + tax + rad + nox,
           family = binomial, data = housing_train)
summary(mod2)
exp(mod2$coefficients)

# From this model we see that chas is now statistically insignificant along with other variables
# including crim, zn, tax, and nox. 
# We do see that increases in number of rooms, lstas, and ptratio all decrease the probability of 
# a home being pricey

# f) Predicting values
predsBoston = data.frame(
  housing_train,
  scores = predict(mod2, type = "response")
)
head(predsBoston)
predsBostonTest = data.frame(
  housing_test,
  scores = predict(glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + tax + rad + nox, family = binomial, data = housing_test), type = "response")
)

# classes
predsBoston$PosNeg05 = ifelse(predsBoston$scores > 0.5 ,1,0)
predsBostonTest$PosNeg05 = ifelse(predsBostonTest$scores > 0.5,1,0)
head(predsBoston)

# g) Confusion Matrix
table(predsBoston$PosNeg05, predsBoston$PriceyHome)
sensitivity = 16/(16+6)
print(sensitivity)
specificity = 355/(355+2)
print(specificity)

table(predsBostonTest$PosNeg05, predsBostonTest$PriceyHome)
sensitivityt = 7/(7+2)
print(sensitivity)
specificityt = 117/(117+1)
print(specificityt)

# You showed us caret on thursday so we can calculate all this using carets confusionMatrix()
library(caret)
confusionMatrix(table(predsBoston$PosNeg05, predsBoston$PriceyHome))
confusionMatrix(table(predsBostonTest$PosNeg05, predsBostonTest$PriceyHome))

# h)
# Our model was failry accurate so using a cutoff of 0.5 was probably a good choice.
# However should we decide to adjust the cutoff we have to consider the 
# trade-offs between increasing specificity and sensitivity in our model.

# i) ROC Curve
library(plotROC)
TrainROC = ggplot(predsBoston, aes(m = scores, 
                     d = PriceyHome)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.99, 0.9, 0.7, 0.6, 0.5, 0.4, 0.1, 0.01))
print(TrainROC)

TestROC = ggplot(predsBostonTest, aes(m = scores, 
                        d = PriceyHome)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(0.99, 0.9, 0.7, 0.6, 0.5, 0.4, 0.1, 0.01))
print(TestROC)

# j) AUC
TrainAUC = calc_auc(TrainROC)
print(TrainAUC)

TestAUC = calc_auc(TestROC)
print(TestAUC)

# It appears that our model fits the data pretty well. If it was underfitted, we can include more data/variables in our model
# If it was overfitting, we can maybe switch to a linear model.


