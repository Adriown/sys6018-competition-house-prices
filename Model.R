setwd("~/Documents/GitHub/sys6018-competition-house-prices")
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
train.data[train.data$GarageArea == 0 & !is.na(train.data$GarageArea),][c(59:61,64:65)] = "No Garage" 
test.data[test.data$GarageArea == 0 & !is.na(test.data$GarageArea),][c(59:61,64:65)] = "No Garage"

#Same logic as for above but for Pool
train.data[train.data$PoolArea == 0 & !is.na(train.data$PoolArea),]$PoolQC = "No Pool" 
test.data[test.data$PoolArea == 0 & !is.na(test.data$PoolArea),]$PoolQC = "No Pool"

#Again but for fence
train.data[is.na(train.data$Fence),]$Fence = "No Fence"
test.data[is.na(test.data$Fence),]$Fence = "No Fence"

#Again but for Miscellaneous Features
train.data[train.data$MiscVal == 0 & !is.na(train.data$MiscVal),]$MiscFeature = "No Feature" 
test.data[test.data$MiscVal == 0 & !is.na(test.data$MiscVal),]$MiscFeature = "No Feature"

train.data <- na.omit(train.data)
test.data <- na.omit(test.data)


##################################
##       PARAMETRIC       ##
##################################

attach(train.data)
str(train.data)

# find the columns with character values  
character <- sapply(train.data, is.character)
train_character= train.data[character]
colnames(train_character)  # name of all the character variables 


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

# fit the model with all explanatory variables 
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
# skewed to the right so we take log of SalePrice
hist(log(SalePrice))


fit.trans  = lm(log(SalePrice)~., data = train.data)
summary(fit.trans)
anova(fit.trans)

par(mfrow=c(2,2)) 
plot(fit.trans)

# Model selection# 
# stepwise model selection 
start<-lm(log(SalePrice) ~1,data= train.data)
end<-lm(log(SalePrice)~.,data= train.data)
result.s<-step(start, scope=list(upper=end), direction="both",trace=FALSE) 
summary(result.s)
anova(result.s)

##################
# Prediction # 
##################
# some cleaning about test data 
test.data = subset(test.data, select = -c(Utilities))

summary(test.data)
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
# Typ 1109 appearance 


test.data$Functional[is.na(test.data$Functional)]<-"Typ"
test.data[test.data$Functional=="Sev",]$Functional<- "Typ"


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
?knn.reg
# Set the value of k at this location
k <- 10
# Getting rid of the columns that don't have numeric data
knn.data <- train.data[as.vector(sapply(train.data, typeof)) != 'character']

# Let's subset to cross validate
sub <- sample(1 : NROW(knn.data), size = NROW(knn.data) / 2)
s1.train <- knn.data[sub,]     # Select subset for cross-validation
s1.valid <- knn.data[-sub,]


# This function will run the non-parametric k-nearest neighbors model
dummy <- knn.reg(train = s1.train[1 : NCOL(s1.train) - 1], test = s1.valid[1 : NCOL(s1.train) - 1], y = s1.train[NCOL(s1.train)]$SalePrice, k = k)
dummy <- knn.reg(train = knn.data[1 : NCOL(knn.data) - 1], y = knn.data[NCOL(knn.data)]$SalePrice, k = k)

s1.valid$PredictedPrice <- dummy$pred
ActualVsPredicted <- s1.valid[c('SalePrice', 'PredictedPrice')]

ggplot(ActualVsPredicted, aes(PredictedPrice, SalePrice)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point()

# Brief line for data cleaning -- mean for the quantitative values
final.test.data <- test.data[as.vector(sapply(test.data, typeof)) != 'character']
meansForReplacement <- colMeans(final.test.data, na.rm = T)

namesWithNAs <- colnames(final.test.data)[apply(final.test.data, 2, function(X) sum(is.na(X))) > 0]
final.test.data$LotFrontage[is.na(final.test.data$LotFrontage)] <- as.numeric(meansForReplacement['LotFrontage'])
final.test.data$MasVnrArea[is.na(final.test.data$MasVnrArea)] <- as.numeric(meansForReplacement['MasVnrArea'])
final.test.data$BsmtFinSF1[is.na(final.test.data$BsmtFinSF1)] <- as.numeric(meansForReplacement['BsmtFinSF1'])
final.test.data$BsmtFinSF2[is.na(final.test.data$BsmtFinSF2)] <- as.numeric(meansForReplacement['BsmtFinSF2'])
final.test.data$BsmtUnfSF[is.na(final.test.data$BsmtUnfSF)] <- as.numeric(meansForReplacement['BsmtUnfSF'])
final.test.data$TotalBsmtSF[is.na(final.test.data$TotalBsmtSF)] <- as.numeric(meansForReplacement['TotalBsmtSF'])
final.test.data$BsmtFullBath[is.na(final.test.data$BsmtFullBath)] <- as.numeric(meansForReplacement['BsmtFullBath'])
final.test.data$BsmtHalfBath[is.na(final.test.data$BsmtHalfBath)] <- as.numeric(meansForReplacement['BsmtHalfBath'])
final.test.data$GarageCars[is.na(final.test.data$GarageCars)] <- as.numeric(meansForReplacement['GarageCars'])
final.test.data$GarageArea[is.na(final.test.data$GarageArea)] <- as.numeric(meansForReplacement['GarageArea'])


# colMean <- apply(final.test.data, 2, function(X){
#   X[is.na(X)] <- meansForReplacement[which(names(meansForReplacement) == )]
# })


test_predictions <- knn.reg(train = knn.data[1 : NCOL(knn.data) - 1], test = test.data[as.vector(sapply(test.data, typeof)) != 'character'], y = knn.data[NCOL(knn.data)]$SalePrice, k = k)
test_predictions <- knn.reg(train = knn.data[1 : NCOL(knn.data) - 1], test = final.test.data[as.vector(sapply(final.test.data, typeof)) != 'character'], y = knn.data[NCOL(knn.data)]$SalePrice, k = k)
submit_this <- test.data['Id']
submit_this$SalePrice <- test_predictions$pred
write_csv(submit_this, 'non_par_kaggle_entry_1.csv')
