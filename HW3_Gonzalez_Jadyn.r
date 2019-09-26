#Jadyn Gonzalez
#Homework Set 3
#Dr. Doosti
#MGSC 310
#September 20, 2019

#############################
#1. Data Exploration

# a) setting working directory to load data into dataframe
setwd("~/Documents/MGSC310")
getwd()
movies = read.csv("movie_metadata.csv")
#displaying dimension of the data
dim(movies)

# b) displaying variable names
names(movies)

# c) check for missing values in budget then removing
sum(is.na(movies$budget))
movies = movies[!is.na(movies$budget),]
#dimension after NaN removal
dim(movies)

# d) counting unique directors with length() and unique()
length(unique(movies$director_name))

# e) using ggplot to create a scatter of imdb_score and budget
library(ggplot2)
ggplot(movies, aes(x = imdb_score,y = budget)) + geom_point()

# f) removing budget outliers
movies = movies[movies$budget<400000000,]
#count of how many movies in dataset
length(unique(movies$movie_title))

# g) plot with cleaned data and added trendline
ggplot(movies, aes(x = imdb_score,y = budget)) + geom_point() + geom_smooth(method = "lm")
#from the line it doesnt look like there is a strong relationship, lets double check
cor(movies$imdb_score, movies$budget)
#this confirms there is not a strong relationship

# h) making subplots
ggplot(movies, aes(x = imdb_score,y = budget)) + geom_point() + facet_wrap(~content_rating, scales = "free") + geom_smooth(method = "lm")
#i

#############################
#2. Data Manipulation
#new variables of gross and budget
movies$grossM = movies$gross/1e+6
movies$budgetM = movies$budget/1e+6
#variable of mian genre
movies$genre_main = do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]

# a) new profit variable and ROI
movies$profitM = movies$gross - movies$budget
movies$ROI = movies$profitM/movies$budget

# b) average ROI
mean(movies$ROI, na.rm = TRUE)

# c) ROI histogram
hist(movies$ROI, breaks = 10)
#regular r plots are not giving a good histogram so we'll use ggplot
ggplot(movies, aes(x = movies$ROI, fill = genre_main)) + geom_histogram() + xlim(-10,10)
#looks much better if we limit the axis but lets remove the outliers

# d) removing outliers
sum(ROI > 10, na.rm = TRUE)
movies = movies[!movies$ROI > 10,]

# e) fixed histogram
hist(movies$ROI)
#now the histogram looks correct
#ggplot of histogram again
ggplot(movies, aes(x = movies$ROI)) + geom_histogram() + xlim(-1,10)

# f) summary
library(doBy)
length(movies$ROI)
length(movies$genre_main)
movieSummary = summaryBy(ROI ~ genre_main, data = movies)
print(movieSummary)
#we see that Fantasy, Musical, Thriller, and Western have highest ROI

# g) average ROI plot
ggplot(movieSummary, aes(y = ROI.mean, x = genre_main)) + geom_point()

#############################
#3. Simple Linear Regression
# a) selecting the test and training sets
set.seed(42)
sampleSize = floor(0.8*nrow(movies))
trainIndex = sample(seq_len(nrow(movies)),size = sampleSize)

train = movies[trainIndex,]
test = movies[-trainIndex,]

# b) dimensions of the sets recall we have 
dim(train)
dim(test)

# c) regress
mod1 = lm(profitM ~ imdb_score,train)
summary(mod1)

# d) coefficients
coef(mod1)
#we get our b0_hat and b1_hat values
#so we get y = -67126324 + 12218267x
#intercept crosses the y axis but for every unit of imdb_score, profit is expected
#to increase by $12,218,267

