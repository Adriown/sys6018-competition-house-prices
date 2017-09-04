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
#################################
attach(train.data)
str(train.data)

# summary(train.data)

is.fact <- sapply(train.data, is.factor)
factor = train.data[is.fact]
colnames(factor)

factor = train.data[is.fact]
apply(train.data[is.fact], 2, function(x)length(unique(x)))
# Utility only has 1 value. 

train.data = subset(train.data, select = -c(Utilities))

# plot SalePrice
hist(SalePrice)
# skewed to the right 
hist(log(SalePrice))


fit  = lm(log(SalePrice)~., data = train.data)
summary(fit)
anova(fit)


fit2 = lm(log(SalePrice)~.-PoolQC-GrLivArea-PavedDrive-OpenPorchSF-MiscVal-PoolArea-KitchenAbvGr-TotRmsAbvGrd-FireplaceQu-Electrical-LowQualFinSF-BsmtHalfBath-FullBath
         -BedroomAbvGr-FullBath-BedroomAbvGr -BsmtCond-SaleType-YrSold-MoSold,data = train.data  )
anova(fit2)
summary(fit2)

plot(fit$fitted.values)

test.data = subset(test.data, select = -c(Utilities))
# roofStyle error but will be fixed 
results <- predict(fit2,newdata= test.data,type='response')



#is.num <- sapply(train.data, is.numeric)
#numeric <- train.data[is.num]
#cov(numeric)



##################################
##       NON - PARAMETRIC       ##
##################################
# So in this portion we are interested in taking the cleaned data and creating a non-parametric
# model that will accurately predict the price of houses from the Kaggle housing competition
?knn.reg
# Set the value of k at this location
k <- 7
# Getting rid of the columns that don't have numeric data
knn.data <- train.data[as.vector(sapply(train.data, typeof)) != 'character']

# Let's subset to cross validate
sub <- sample(1 : NROW(knn.data), size = NROW(knn.data) / 2)
s1.train <- knn.data[sub,]     # Select subset for cross-validation
s1.valid <- knn.data[-sub,]

# Feature selection is difficult. Going to look quickly at correlations to see if I can pull out any information
install.packages('corrplot')
library(corrplot)
M <- cor(knn.data)
corrplot(M, method="color")

# Adding MoSold and YrSold together for a new column
knn.data$YrMoSold <- as.integer(as.Date(paste0(knn.data$YrSold, '-', knn.data$MoSold, '-01')))

# These are columns which are either unrelated to the data, categorical in nature, are the response variable, or are being expressed with another column
colsToRemove <- c('Id', 'MSSubClass', 'MiscVal', 'MoSold', 'YrSold', 'SalePrice')
colsToInclude <- c('OverallQual', 'GarageCars', 'BedroomAbvGr', 'BsmtFullBath', 'TotRmsAbvGrd', 'BsmtHalfBath')

# This function will run the non-parametric k-nearest neighbors model -- this form will let you do cross-validation yourself
dummy <- knn.reg(train = s1.train[1 : NCOL(s1.train) - 1], test = s1.valid[1 : NCOL(s1.train) - 1], y = s1.train[NCOL(s1.train)]$SalePrice, k = k)
# This portion here will do its own cross-validation on your training set
dummy <- knn.reg(train = knn.data[(colnames(knn.data) %in% colsToInclude)], 
                 y = knn.data$SalePrice, 
                 k = k)

# Here we're looking for the k-value that gives us the lowest squared error on our predictions
flex_finding <- unlist(lapply(X = 1:50, function(X){
  knn.reg(train =  knn.data[(colnames(knn.data) %in% colsToInclude)], 
          y = knn.data$SalePrice, 
          k = X)$PRESS
}))


# Here we're looking for the feature selection that gives us the best PRESS for k = 10
combinations <- as.matrix(expand.grid(3, 24, 20, 16, 22, 17))
flex_finding <- unlist(lapply(X = 1 : NROW(combinations), function(X){
  knn.reg(train =  knn.data[!(colnames(knn.data) %in% colsToRemove)][as.vector(combinations[X, ])],
  y = knn.data$SalePrice,
  k = 7)$PRESS
}))



colnames(knn.data[!(colnames(knn.data) %in% colsToRemove)])[as.vector(combinations[which(flex_finding == min(flex_finding)),])]
as.vector(combinations[which(flex_finding == min(flex_finding)),])
which(flex_finding == min(flex_finding))
min(flex_finding)
# k=7 best, with PRESS = 2.67E12
# k=7, 1.831452e12 (with just 3 features)
# LOWEST I COULD GET WAS k=7, 6 features (iterative), at PRESS = 1.59E12


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
