library(readr)
library(dplyr)

library(FNN)
library(ggplot2)

train.data <- read_csv("Train.csv")
test.data <-  read_csv("Test.csv")


#NA in the context of the alley variable means that 
#there is no alley access, not that the data was missing
#so we replace NA with the word "No Alley"
train.data[is.na(train.data$Alley),]$Alley = "No Alley"
test.data[is.na(test.data$Alley),]$Alley = "No Alley"

#Same logic as for alley, but for BsmtQual, BsmtCond,
#BsmtExposure, BsmtFinType1, BsmtFinType2
train.data[train.data$TotalBsmtSF == 0 & !is.na(train.data$TotalBsmtSF),][c(31:34,36)] = "No Bsmt" 
test.data[test.data$TotalBsmtSF == 0 & !is.na(test.data$TotalBsmtSF),][c(31:34,36)] = "No Bsmt"

#Same logic as for above but for fireplace
train.data[train.data$Fireplaces == 0,]$FireplaceQu = "No Fireplace"
test.data[test.data$Fireplaces == 0,]$FireplaceQu = "No Fireplace"

#Same but for garage
train.data[train.data$GarageArea == 0 & !is.na(train.data$GarageArea),][c(59:60,64:65)] = "No Garage" 
test.data[test.data$GarageArea == 0 & !is.na(test.data$GarageArea),][c(59:60,64:65)] = "No Garage"
train.data[train.data$GarageArea == 0 & !is.na(train.data$GarageArea),][61] = 0 
test.data[test.data$GarageArea == 0 & !is.na(test.data$GarageArea),][61] = 0

#Same logic as for above but for Pool
train.data[train.data$PoolArea == 0 & !is.na(train.data$PoolArea),]$PoolQC = "No Pool" 
test.data[test.data$PoolArea == 0 & !is.na(test.data$PoolArea),]$PoolQC = "No Pool"

#Again but for fence
train.data[is.na(train.data$Fence),]$Fence = "No Fence"
test.data[is.na(test.data$Fence),]$Fence = "No Fence"

#Again but for Miscellaneous Features
train.data[train.data$MiscVal == 0 & !is.na(train.data$MiscVal),]$MiscFeature = "No Feature" 
test.data[test.data$MiscVal == 0 & !is.na(test.data$MiscVal),]$MiscFeature = "No Feature"



apply(test.data,2,function(X) sum(is.na(X)))

train.data <- na.omit(train.data)

##################
# Prediction # 
##################
#Cleaning of the test data 
test.data = subset(test.data, select = -c(Utilities))
summary(test.data)
#replace missing values from numerical variables with their means
test.data$LotFrontage[is.na(test.data$LotFrontage)] <- mean(test.data$LotFrontage,na.rm=T)
test.data$MasVnrArea[is.na(test.data$MasVnrArea)] <- mean(test.data$MasVnrArea,na.rm=T)
test.data$BsmtFinSF1[is.na(test.data$BsmtFinSF1)] <- mean(test.data$BsmtFinSF1,na.rm=T)
test.data$BsmtFinSF2[is.na(test.data$BsmtFinSF2)] <- mean(test.data$BsmtFinSF2,na.rm=T)
test.data$BsmtUnfSF[is.na(test.data$BsmtUnfSF)] <- mean(test.data$BsmtUnfSF,na.rm=T)
test.data$TotalBsmtSF[is.na(test.data$TotalBsmtSF)] <- mean(test.data$TotalBsmtSF,na.rm=T)
test.data$BsmtHalfBath[is.na(test.data$BsmtHalfBath)] <- mean(test.data$BsmtHalfBath,na.rm=T)
test.data$BsmtFullBath[is.na(test.data$BsmtFullBath)] <- mean(test.data$BsmtFullBath,na.rm=T)
test.data$GarageCars[is.na(test.data$GarageCars)] <- mean(test.data$GarageCars,na.rm=T)
test.data$GarageArea[is.na(test.data$GarageArea)] <- mean(test.data$GarageArea,na.rm=T)
aggregate(Id ~ BsmtExposure, train.data, function(x) length(unique(x)))
test.data$BsmtExposure[is.na(test.data$BsmtExposure)] <- "No"
aggregate(Id ~ KitchenQual, train.data, function(x) length(unique(x)))
test.data$KitchenQual[is.na(test.data$KitchenQual)] <- "TA"
aggregate(Id ~ MSZoning, train.data, function(x) length(unique(x)))
test.data$MSZoning[is.na(test.data$MSZoning)] <- "RL"
aggregate(Id ~ BsmtFinType1,train.data, function(x) length(unique(x)))
test.data$BsmtFinType1[is.na(test.data$BsmtFinType1)] <- "GLQ"
aggregate(Id ~ SaleType,train.data, function(x) length(unique(x)))
test.data$SaleType[is.na(test.data$SaleType)] <- "WD"
aggregate(Id ~ Functional, train.data, function(x) length(unique(x)))
#Replace missing values in categorical data with the Mode.
test.data$MasVnrType[is.na(test.data$MasVnrType)] <- "None"
which.max(table(test.data$BsmtQual))
test.data$BsmtQual[is.na(test.data$BsmtQual)] <- "TA"
which.max(table(test.data$BsmtCond))
test.data$BsmtCond[is.na(test.data$BsmtCond)] <- "TA"
which.max(table(test.data$BsmtFinType2))
test.data$BsmtFinType2[is.na(test.data$BsmtFinType2)] <- "Unf"
test.data$GarageYrBlt[is.na(test.data$GarageYrBlt)] <- test.data$YearBuilt[is.na(test.data$GarageYrBlt)]
test.data$GarageFinish[is.na(test.data$GarageFinish)] <- "Unf"
test.data$GarageQual[is.na(test.data$GarageQual)] <- "TA"
test.data$GarageCond[is.na(test.data$GarageCond)] <- "TA"
test.data$Functional[is.na(test.data$Functional)]<-"Typ"
test.data[test.data$Functional=="Sev",]$Functional<- "Typ"



##################################
##       PARAMETRIC       ##
##################################
attach(train.data)
str(train.data)
# find the columns with character values  
character <- sapply(train.data, is.character)
train_character= train.data[character]
colnames(train_character)  # name of all the character variables 
# find how many levels each variable has 
apply(train_character, 2, function(x)length(unique(x))) 
# Utility only has 1 value. 
# As Utility only contains 1 value so we decide to remove Utilities 
train.data = subset(train.data, select = -c(Utilities))
# Have some overview of the data 
numeric <- sapply(train.data, is.numeric)
train_numeric= train.data[numeric]
attach(train_numeric)
boxplot(train_numeric[1:36])
boxplot(SalePrice,main="boxplot for y")
dev.off()
# correlation matrix for all the numeric variables 
round(cor(train_numeric),3)
# check multicolinearity 
library(faraway)
vif(train_numeric)
# Some multicolinearities exist so we will not include all of the correlated varibles in the model
# try fitting the model with all explanatory variables 
fit  = lm(SalePrice~., data = train.data)
summary(fit)
anova(fit)
# check assmuptions for linear regression model
par(mfrow=c(2,2))
plot(fit) # generally okay 
# transformation of response variable SalePrice 
library(MASS)
boxcox(fit,data=train.data) # lamda very close to 0 so take log transformation 
# further plot histogram of SalePrice
hist(SalePrice)   
# skewed to the right so we take log of SalePrice to make distribution of SalepPrice more normal
hist(log(SalePrice))
# use the transformed SalePrice as our response varible 
fit.trans  = lm(log(SalePrice)~., data = train.data)
summary(fit.trans)
anova(fit.trans)
par(mfrow=c(2,2)) 
plot(fit.trans) 
# From the plot, it looks like 434 685 525 are potential outliers that will influence the model 
# remove outliers observation 434 685 525
train.data = train.data[-c(434,685,525), ]
# Model selection # 
# stepwise model selection 
# We used stepwise model selection. R will run back and forth and select one best from all the possible models
# based on criteria like AIC BIC... 
start<-lm(log(SalePrice) ~1,data= train.data)
end<-lm(log(SalePrice)~.,data= train.data)
result.s<-step(start, scope=list(upper=end), direction="both",trace=FALSE) 
summary(result.s)
anova(result.s)
# We check the assumptions again by plotting the model fitting 
par(mfrow=c(2,2)) 
plot(result.s) 

results <- predict(result.s,newdata= test.data,type='response')
results[is.na(results)]
par_pred <- exp(results)
table = data.frame(test.data$Id,par_pred)
write.table(table,file="par_kaggle_entry_1.csv",sep = ',', row.names = F,col.names = c('ID','SalePrice'))

##################################
##       NON - PARAMETRIC       ##
##################################
# So in this portion we are interested in taking the cleaned data and creating a non-parametric
# model that will accurately predict the price of houses from the Kaggle housing competition
# Going to use knn regression as this is not a classification problem; knn is a powerful non-parametric
# approach which we think will work well as it doesn't make any strong assumptions about the form of the 
# underlying relationship, only assuming that similarly-priced things will have other attributes that are
# closely related to one another. The potential downside is that with ~1,200 observations, we won't really
# have enough training data to get past the problem of multi-dimensionality that occurs with these non-
# parametric methods. We implemented cross-validation for the training set and tried to document the 
# code well. Functions were used in lapply statements but otherwise there wasn't enough redundancy or 
# complexity to warrant writing functions. And commented-out code largely contains the thought process
# that took me to my eventual model selection.
?knn.reg
# Set the value of k at this location (I found that k=7 worked best by varying my k values around)
k <- 7
# Getting rid of the columns that don't have numeric data (as we can only find Euclidean distance on numeric cooumns)
knn.data <- train.data[as.vector(sapply(train.data, typeof)) != 'character']
# Let's subset to cross validate
# sub <- sample(1 : NROW(knn.data), size = NROW(knn.data) / 2)
# s1.train <- knn.data[sub,]     # Select subset for cross-validation
# s1.valid <- knn.data[-sub,]
# Feature selection is difficult. Going to look quickly at correlations to see if I can pull out any information
# Part of feature selection was figuring out which columns are most closely correlated with the price. I ended up not
# having this be my primary method though
# install.packages('corrplot')
# library(corrplot)
# M <- cor(knn.data)
# corrplot(M, method="color")
# Adding MoSold and YrSold together for a new column
# Now it's continuous data that could be interesting
knn.data$YrMoSold <- as.integer(as.Date(paste0(knn.data$YrSold, '-', knn.data$MoSold, '-01')))
# These are columns which are either unrelated to the data, categorical in nature, are the response variable, or are being expressed with another column
# colsToRemove <- c('Id', 'MSSubClass', 'MiscVal', 'MoSold', 'YrSold', 'SalePrice')
# The followest columns were the best for predicting the house prices
colsToInclude <- c('OverallQual', 'GarageCars', 'BedroomAbvGr', 'BsmtFullBath', 'TotRmsAbvGrd', 'BsmtHalfBath')
# This function will run the non-parametric k-nearest neighbors model -- this form will let you do cross-validation yourself
# dummy <- knn.reg(train = s1.train[1 : NCOL(s1.train) - 1], test = s1.valid[1 : NCOL(s1.train) - 1], y = s1.train[NCOL(s1.train)]$SalePrice, k = k)
# This portion here will do its own cross-validation on your training set
dummy <- knn.reg(train = knn.data[(colnames(knn.data) %in% colsToInclude)],
                 y = knn.data$SalePrice,
                 k = k)
# Here we're looking for the k-value that gives us the lowest squared error on our predictions
# I varied k from 1 to 50 and found the value that gave the lowest Predicted Residual Sum of Squares (PRESS)
# It happened to be k=7
# flex_finding <- unlist(lapply(X = 1:50, function(X){
#   knn.reg(train =  knn.data[(colnames(knn.data) %in% colsToInclude)], 
#           y = knn.data$SalePrice, 
#           k = X)$PRESS
# }))
# Here we're looking for the feature selection that gives us the best PRESS for k = 7
# At first I looked for the 4 columns that gave the best Predicted Residual Sum of Squares (PRESS)
# Then I kept adding columns until the PRESS stopped decreasing (which happened after 6 columns)
# combinations <- as.matrix(expand.grid(3, 24, 20, 16, 22, 17))
# flex_finding <- unlist(lapply(X = 1 : NROW(combinations), function(X){
#   knn.reg(train =  knn.data[!(colnames(knn.data) %in% colsToRemove)][as.vector(combinations[X, ])],
#   y = knn.data$SalePrice,
#   k = 7)$PRESS
# }))
#colnames(knn.data[!(colnames(knn.data) %in% colsToRemove)])[as.vector(combinations[which(flex_finding == min(flex_finding)),])]
# These columns provided the best predictions!
# Output: "OverallQual"  "GarageCars"   "BedroomAbvGr" "BsmtFullBath" "TotRmsAbvGrd" "BsmtHalfBath"
# as.vector(combinations[which(flex_finding == min(flex_finding)),])
# which(flex_finding == min(flex_finding))
# min(flex_finding)
# k=7 best, with PRESS = 2.67E12
# k=7, 1.831452e12 (with just 3 features)
# LOWEST I COULD GET WAS k=7, 6 features (iterative), at PRESS = 1.59E12
# Looking at my validation test
# s1.valid$PredictedPrice <- dummy$pred
# ActualVsPredicted <- s1.valid[c('SalePrice', 'PredictedPrice')]
# Low-level sanity plot of my predictions of price vs actual sale price
# ggplot(ActualVsPredicted, aes(PredictedPrice, SalePrice)) + 
#   geom_abline(slope = 1, intercept = 0) + 
#   geom_point()
# Brief line for data cleaning -- mean for the quantitative values
# Copy over test data
# Ignore character columns
final.test.data <- test.data[as.vector(sapply(test.data, typeof)) != 'character']
meansForReplacement <- colMeans(final.test.data, na.rm = T)
#These are columns where we need some pre-processing of the data for the model to work (deal with the missing values using mean)
namesWithNAs <- colnames(final.test.data)[apply(final.test.data, 2, function(X) sum(is.na(X))) > 0]
# colMean <- apply(final.test.data, 2, function(X){
#   X[is.na(X)] <- meansForReplacement[which(names(meansForReplacement) == )]
# })
# Now produce predictions
#test_predictions <- knn.reg(train = knn.data[1 : NCOL(knn.data) - 1], test = test.data[as.vector(sapply(test.data, typeof)) != 'character'], y = knn.data[NCOL(knn.data)]$SalePrice, k = k)

test_predictions <- knn.reg(train = knn.data[(colnames(knn.data) %in% colsToInclude)], 
                            test = final.test.data[(colnames(final.test.data) %in% colsToInclude)], 
                            y = knn.data$SalePrice, 
                            k = k)
submit_this <- test.data['Id']
submit_this$SalePrice <- test_predictions$pred
# Output to csv
write_csv(submit_this, 'Competition1-1_non_par_kaggle_entry.csv')
