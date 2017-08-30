library(readr)
library(dplyr)

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
