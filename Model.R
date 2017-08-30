setwd("~/Documents/GitHub/sys6018-competition-house-prices")
library(readr)
library(dplyr)

train.data <- read.csv("train.csv")
test.data <-  read.csv("test.csv")
badcols = apply(train.data,2,function(X) sum(is.na(X))>1000)

train.data = train.data[!as.vector(badcols)]
test.data = test.data[!as.vector(head(badcols,-1))]


train.data <- na.omit(train.data)
test.data <- na.omit(test.data)
attach(train.data)
str(train.data)

# summary(train.data)

is.fact <- sapply(train.data, is.factor)
factor = train.data[is.fact]
colnames(factor)
contrasts(train.data[is.fact])

summary(train.data[is.fact])
factor = train.data[is.fact]
apply(train.data[is.fact], 2, function(x)length(unique(x)))
# Utility only has 1 value. 

train.data = subset(train.data, select = -c(Utilities))

subset(df, select = c(a,c))
# plot SalePrice
hist(SalePrice)
# skewed to the right 


lm(SalePrice~., data = train.data)




