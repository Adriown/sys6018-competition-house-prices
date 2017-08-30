library(readr)
library(dplyr)

train.data <- read_csv("Train.csv")
test.data <-  read_csv("Test.csv")
badcols = apply(train.data,2,function(X) sum(is.na(X))>1000)

train.data = train.data[!as.vector(badcols)]
test.data = test.data[!as.vector(head(badcols,-1))]


train.data <- na.omit(train.data)
test.data <- na.omit(test.data)
