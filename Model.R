library(readr)
library(dplyr)
library(FNN)

train.data <- read_csv("Train.csv")
test.data <-  read_csv("Test.csv")
badcols = apply(train.data,2,function(X) sum(is.na(X))>1000)

train.data = train.data[!as.vector(badcols)]
test.data = test.data[!as.vector(head(badcols,-1))]


train.data <- na.omit(train.data)
test.data <- na.omit(test.data)







##################################
##       NON - PARAMETRIC       ##
##################################
# So in this portion we are interested in taking the cleaned data and creating a non-parametric
# model that will accurately predict the price of houses from the Kaggle housing competition
?knn.reg
# Set the value of k at this location
k <- 10
# Getting rid of the columns that don't have numeric data
knn.data <- train.data[as.vector(sapply(train.data, typeof)) != 'character']



# This function will run the non-parametric k-nearest neighbors model
dummy <- knn.reg(train = knn.data[1 : NCOL(knn.data) - 1], y = knn.data[NCOL(knn.data)]$SalePrice, k = k)




install.packages('chemometrics')
if(require(chemometrics)){
  data(PAC);
  pac.knn<- knn.reg(PAC$X, y=PAC$y, k=3);
  
  plot(PAC$y, pac.knn$pred, xlab="y", ylab=expression(hat(y)))
} 
