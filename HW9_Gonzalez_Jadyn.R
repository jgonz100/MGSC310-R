# Jadyn Gonzalez
# Dr. Doosti
# Problem Set 9
# November 15, 2019

##########################################
# Question 1: Tree models with Auto data #
##########################################
data("Auto")
# a)
set.seed(2019)

# b) Test and train set 75% split
trainidx = sample(1:nrow(Auto),size=0.75*nrow(Auto))
train = Auto[trainidx,]
test = Auto[-trainidx,]

# c) Tree regression model against mpg
library(tree)
treeMod = tree(mpg ~ cylinders + displacement
               + horsepower + weight + acceleration
               + year + origin, 
               data = train)

# d) Plot tree
plot(treeMod)
text(treeMod, pretty=0)

# e) Interpretation
# In the tree we see that cylinders is the most important variable in deciding mpg.
# The nodes at the bottom of the tree, the leaves, they are the predictions based on the averages
# of the region decided by each split.
# If the number of cylinders is less than 4.5 we see that weight and year are the next best 
# variables for predicting mpg. If cylinders is greater than 4.5, horsepower followed by year and
# weight best determines mpg. The predictions at the leaves are determined by following the path
# from the parent node cylinders.

# f) Predictions and MSE
predsTrain = predict(treeMod)
predsTest = predict(treeMod, newdata = test)

MSE = function(p,t){
  mean((t-p)^2)
}
MSE(predsTrain,train$mpg)
MSE(predsTest,test$mpg)

# g) CV for best tree size
cvTree = cv.tree(treeMod)
cvTree
bestIdx = which.min(cvTree$dev)
cvTree$size[bestIdx] # Best tree size == 9

# h) Pruning Tree
prunedTree = prune.tree(treeMod, best = 9)
predsTrain_pruned = predict(prunedTree)
predsTest_pruned = predict(prunedTree, newdata = test)

predsTrain_pruned = predict(prunedTree)
predsTest_pruned = predict(prunedTree, newdata = test)

MSE = function(p,t){
  mean((t-p)^2)
}
MSE(predsTrain_pruned,train$mpg)
MSE(predsTest_pruned,test$mpg)
# Pruned tree is the same as the original tree

# i) Bagging model
library(randomForest)
bagMod = randomForest(mpg ~ cylinders + displacement
                      + horsepower + weight + acceleration
                      + year + origin,
                      data = train,
                      ntree = 500,
                      mtry = 7, # mtry = 7 9 variables - mpg and name = 7
                      importance = TRUE)

predsBagg_train = predict(bagMod)
predsBagg_test = predict(bagMod, newdata = test)

MSE(predsBagg_train, train$mpg)
MSE(predsBagg_test, test$mpg)
# Bagging significantly lowered MSE in both sets

# j) Random Forest
rfMod = randomForest(mpg ~ cylinders + displacement
                     + horsepower + weight + acceleration
                     + year + origin,
                     data = train,
                     ntree = 500,
                     mtry = 3,
                     importance = TRUE)

# k) Mtry = 3 means that to introduce variation in the splits, each time a split is 
# considered in the tree the split will be decided on a random selection of three variables.

# l) Random Forest predictions
predsRF_train = predict(rfMod)
predsRF_test = predict(rfMod, newdata = test)

MSE(predsRF_train, train$mpg)
MSE(predsRF_test, test$mpg)

# Train MSE is not as good as bagging but we get the lowest test MSE with the random
# forest approach

# m) Visualization of important
importance(rfMod)
varImpPlot(rfMod)
# In the random forest model we see that year is the most important variable, this is different
# from our original tree where cylinders was most important

# n) The original tree produced MSEs that were significantlly higher than MSEs from the
# bagging and random forest models. This is expected since bagging reduces variance and random forest
# introduces variation in the model to increase predictive power. In our models random forest
# outperformed the bagging model due to the added variation. Each tree in the random forest
# is independent of others and results in better bias-variance tradeoff and higher
# predictive power.