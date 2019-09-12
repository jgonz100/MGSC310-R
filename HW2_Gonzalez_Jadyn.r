#Jadyn Gonzalez
#Problem Set 2
#Chapter 2 Exercise 8 and 10

#***********************************#
#Question 8
install.packages('tinytex')
tinytex::install_tinytex()
#a
setwd("~/Documents/MGSC310")
college = read.csv("College.csv")

rownames(college) = college[,1]
View(college)

college = college[,-1]
View(college)

#b

#i
summary(college)

#ii
pairs(college[,1:10])

#iii
plot(college$Private, college$Outstate, xlab = "Private University", ylab = "Out of State Tuition", main = "Private vs Outstate Plot")

#iv
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Private University", ylab = "Out of State Tuition", main = "Elite vs Outstate Plot")

#v
par(mfrow = c(2,2))
hist(college$Top25perc, col = 2, xlab = "Top25Perc", ylab = "Count")
hist(college$Top10perc, col = 3, xlab = "Top10Perc", ylab = "Count")
hist(college$Accept, col = 4, xlab = "Accept", ylab = "Count")
hist(college$Grad.Rate, col = 5, xlab = "Grad Rate", ylab = "Count")

#vi
summary(college$Top25perc)
summary(college$Top10perc)
summary(college$PhD)
summary(college$Grad.Rate)

  #PhD and Grade.Rate exceed 100%?? possible anaomalies
anom_phd = college[college$PhD == 103,]
nrow(anom_phd)
anom_phd
#PhD for Texas A&M is 103

anom_grad = college[college$Grad.Rate == 118,]
nrow(anom_grad)
anom_grad
#Grad.rate for Cazenovia College is 118

#***********************************#
#Question 10

#a
library(MASS)
Boston
?Boston

dim(Boston)
summary(Boston)

#b
pairs(Boston)
#we can see chas is a binary var and crim contains outliers
#variables may need to be cleansed

#c
cor(Boston, use = "pairwise.complete.obs")
#yes such as ptratio, rad, tax, lstat, age, indus, and nox

#d
par(mfrow = c(2,2))
hist(Boston$crim, main = "Crime Rates")
hist(Boston$crim, main = "Crime Rates", ylim = c(0,40))
hist(Boston$tax, main = "Tax Rates")
hist(Boston$ptratio, main = "Pupil-Teacher Ratio")
# we see crime rates is heavily skewed
# tax rates has some high outliers
# pupil-teacher ratio doesnt appear to have significant outliers

#e
summary(Boston$chas == 1)
#35

#f
median(Boston$ptratio)

#g
which.min(Boston$medv)

#h
summary(Boston$rm > 8)

idx = Boston$rm > 8
summary(idx)
summary(subset(Boston, rm > 8))
summary(Boston)


