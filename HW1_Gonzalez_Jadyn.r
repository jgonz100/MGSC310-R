#Jadyn Gonzalez
#Problem Set 1 Question 3

#a
getwd()
setwd("~/Documents/MGSC310")
getwd()

#b
id = c(seq(1,150))
id
id[2]

#c
set.seed(29) #SID 2290329
netflix = rnorm(150, 20, 5)

#d
set.seed(29)
hulu = runif(150, 0, 15)

#e
hist(netflix)
hist(hulu)

#f
subscription = as.factor(c("Yes", "No"))
is.factor(subscription)

#g
set.seed(29)
amazon = c(sample(levels(subscription), 150, replace = TRUE))
amazon
amazon[1:20]

#h
sum(amazon == "Yes")

#i
sum(netflix > 20 & amazon == "No")

#j
sum((hulu & netflix) < 12 & amazon == "Yes")

#k
id[amazon == "Yes"]
