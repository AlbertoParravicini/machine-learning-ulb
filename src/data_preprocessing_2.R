rm(list=ls())

library(readr)
library(purrr)
library(tidyr)
library(rbokeh)
library(plyr)
library(tidyr)
library(ggplot2)
library(sqldf)
library(xtable)
library(dplyr)

#####################
# UTILITY FUNCTIONS #
#####################

# Fill the lot frontage field by using the median of the values.
# of the houses in the neighbourhood.
# To increase randomness, the variance of the neighborhood is also considered.
# This function will be used later to fill the train set, as in crossvalidation
# we don't want to leak information.
fill_lot_frontage <- function(data_to_fill, data_to_sample)
{
  # For each neighbourhood, compute the median frontage field area.
  # Fill the missing values.
  front_area <- train %>% dplyr::group_by(Neighborhood) %>% dplyr::summarise(median_area=median(LotFrontage, na.rm = T), dev=sd(LotFrontage, na.rm=T))
  
  na_indices <- which(is.na(data_to_fill$LotFrontage))
  for(i in na_indices)
  {
    data_to_fill[i, ]$LotFrontage <- floor(rnorm(n=1,
                                           mean=subset(front_area, Neighborhood==data_to_fill[i, ]$Neighborhood)$median_area,
                                           sd=subset(front_area, Neighborhood==data_to_fill[i, ]$Neighborhood)$dev))
  }
  data_to_fill$LotFrontage <- if_else(data_to_fill$LotFrontage < 21, 21, data_to_fill$LotFrontage)
  return(data_to_fill)
}


#############
# READ DATA #
#############

# Import the data
train <- read_csv("~/machine-learning-ulb/data/train.csv")
test <- read_csv("~/machine-learning-ulb/data/test.csv")

all_data <- rbind(train[, -ncol(train)], test)

# Print header
print(head(train))
# Count column types
table(sapply(train, class))

#####################
# HANDLE NA ########
####################
# This can be done just once for the train and the test set.

# Count NA's per column.
na_train <- data.frame(col=colnames(all_data),
                       col_type=sapply(all_data, class),
                       na_sum=apply(X = all_data, MARGIN = 2, FUN=function(x) sum(is.na(x))),
                       na_perc=apply(X = all_data, MARGIN = 2, FUN=function(x) sum(is.na(x))/length(x)), row.names = NULL)
# Visualise factor count for each categorical feature.
lapply(keep(all_data, is.character), table)
# Visualize distributions for numerical features.
lapply(keep(all_data, is.numeric), summary)

apply(X = all_data, MARGIN = 2, FUN=function(x) sum(is.na(x)))

# Replace MSZoning, by looking at the most frequent classes of buildings of the same kind.
all_data[is.na(all_data$MSZoning),c('MSZoning','MSSubClass')]
which(is.na(all_data$MSZoning))
table(train$MSZoning, train$MSSubClass)
all_data$MSZoning[c(2217, 2905)] <- 'RL'
all_data$MSZoning[c(1916, 2251)] <- 'RM'

# Replace "Alley" == NA with a default value.
all_data$Alley <- if_else(is.na(all_data$Alley), "None", all_data$Alley)

# Handle features with few missing data, and not connected to each other (e.g. garage):
# Utilities, Electrical, KitchenQual, Functional, SaleType

# Drop the Utilities feature, all the data in the test set have the same value.
all_data$Utilities <- NULL

# Electrical and KitchenQual can be replaced with the most common value, in the training set.
# It would be better to use a random replacement from a discrete distribution where the probabilities
# are given by the frequency of each option, and do this at a later stage (i.e. crossvalidation).
# However we have just 1 missing value, so it doesn't matter.
all_data$Electrical <- if_else(is.na(all_data$Electrical), "SBrkr", all_data$Electrical)
all_data$KitchenQual <- if_else(is.na(all_data$KitchenQual), "TA", all_data$KitchenQual)

# Infer the SaleType from the SaleCondition.
all_data[is.na(all_data$SaleType), ]$SaleCondition
table(train$SaleType, train$SaleCondition)
all_data$SaleType <- if_else(is.na(all_data$SaleType), "WD", all_data$SaleType)
# Replace Functional with the most common value.
table(train$Functional)
all_data$Functional <- if_else(is.na(all_data$Functional), "Typ", all_data$Functional)

# Exerior Material, look at the Exterior quality.
table(train$Exterior1st, train$ExterQual) # TA
table(train$Exterior1st, train$ExterCond) # TA
all_data$Exterior1st <- if_else(is.na(all_data$Exterior1st), "VinylSd", all_data$Exterior1st)
all_data$Exterior2nd <- if_else(is.na(all_data$Exterior2nd), "VinylSd", all_data$Exterior2nd)

# MasVnrType: 
# See if the data are missing from the same houses.
all_data[is.na(all_data$MasVnrArea) | is.na(all_data$MasVnrType), c("Id", "MasVnrArea", "MasVnrType")]
# Handle the special case
# Evaluate for each type of masonry the median area, and the number of occurrences.
# The missing type has value of 198, and "Stone" is the closest one. 
all_data %>% dplyr::group_by(MasVnrType) %>% dplyr::summarise(area=median(MasVnrArea, na.rm = T), n=n()) 
all_data[2611, 'MasVnrType'] = 'Stone'

# Substitute the pairs that are both NA's with <0, "None">
all_data$MasVnrType[is.na(all_data$MasVnrType)] = 'None'
all_data$MasVnrArea[is.na(all_data$MasVnrArea)] = 0


# Handle Basement
# There is one row with missing BsmtFinType2. 
# We can infer it from:
table(train$BsmtFinType1, train$BsmtFinType2)
all_data$BsmtFinType2[333] = "Unf"

# From the data description, NA means that no basement is present.
# We can use a default value, and set to 0 the size of the basement.
all_data$BsmtFinType1[is.na(all_data$BsmtFinType1)] = 'None'
all_data$BsmtFinSF1[is.na(all_data$BsmtFinSF1)] = 0
all_data$BsmtFinType2[is.na(all_data$BsmtFinType2)] = 'None'
all_data$BsmtFinSF2[is.na(all_data$BsmtFinSF2)] = 0
all_data$BsmtUnfSF[is.na(all_data$BsmtUnfSF)] = 0
all_data$TotalBsmtSF[is.na(all_data$TotalBsmtSF)] = 0
all_data$BsmtFullBath[is.na(all_data$BsmtFullBath)] = 0
all_data$BsmtHalfBath[is.na(all_data$BsmtHalfBath)] = 0
all_data$BsmtQual[is.na(all_data$BsmtQual)] = "None"
all_data$BsmtCond[is.na(all_data$BsmtCond)] = "None"
all_data$BsmtExposure[is.na(all_data$BsmtExposure)] = "None"


# Fireplace: NA's means that there is no fireplace.

# Make sure there are no dirty data.
subset(all_data, is.na(FireplaceQu) & Fireplaces > 0)
all_data$FireplaceQu[is.na(all_data$FireplaceQu)] = 'None'


# PoolQC: NA's means that there is no pool.

# Make sure there are no dirty data.
subset(all_data, is.na(PoolQC) & PoolArea > 0)[, c("Id", "PoolArea")]
# 3 rows have a pool with no pool quality. Infer it from the size and from the house quality.
all_data %>% dplyr::group_by(PoolQC) %>% dplyr::summarise(area=median(PoolArea, na.rm = T))
# Infer the most likely data.
all_data[2421, 'PoolQC'] = 'Ex'
all_data[2504, 'PoolQC'] = 'Ex'
all_data[2600, 'PoolQC'] = 'Fa'
all_data$PoolQC[is.na(all_data$PoolQC)] = 'None'


# Fence: NA's means that there is no fence.
all_data$Fence[is.na(all_data$Fence)] = 'None'

# MiscFeature: NA's means that there is no miscellaneous feature.
all_data$MiscFeature[is.na(all_data$MiscFeature)] = 'None'


# Garage: 
# In most cases, NA's are due to the lack of a garage.
# If GarageType is NA, we can put all the other associated variables to "None" or 0.
# The Exception is GarageYrBlt, for which no meaningful default exists.
# We can replace it with the year in which the house was built, 
# and later add more informative features (e.g. GaragePresent = Y/N, or GarageBuiltAfter = GarageYrBlt - YearBuilt)
garage_na <- which(is.na(all_data$GarageType))
all_data$GarageYrBlt <- if_else(is.na(all_data$GarageType), all_data$YearBuilt, all_data$GarageYrBlt)
all_data$GarageType[garage_na] <- "None"
all_data$GarageFinish[garage_na] <- "None"
all_data$GarageQual[garage_na] <- "None"
all_data$GarageCond[garage_na] <- "None"

# For the remaining 2 rows, we can infer the values from the garage type.
which(is.na(all_data$GarageQual))
table(train$GarageQual, train$GarageType)
table(train$GarageCond, train$GarageType)
table(train$GarageFinish, train$GarageType)
table(train$GarageCars, train$GarageType)
all_data$GarageQual[c(2127, 2577)] <- "TA"
all_data$GarageCond[c(2127, 2577)] <- "TA"
all_data$GarageFinish[c(2127, 2577)] <- "Unf"
all_data$GarageCars[2577] <- 2
all_data %>% dplyr::group_by(GarageType) %>% dplyr::summarise(median(GarageArea, na.rm = T))
all_data$GarageArea[2577] <- 399
all_data$GarageYrBlt[c(2127, 2577)] <- all_data$YearBuilt[c(2127, 2577)]

# Note: one garage was built in 2207 (!)
# We can replace this value with the year in which the house was built.
all_data$GarageYrBlt[2593] <- all_data$YearBuilt[2593]

# FINAL CHECK
apply(X = all_data, MARGIN = 2, FUN=function(x) sum(is.na(x)))


#####################
# OUTLIER DETECTION #
#####################

# There are some rows in the training set with really large values for some numerical features.
# These values are not encountered in the test set, and don't represent a meaningful patter across the training set.
# We can remove these rows without too many problems.

figure() %>% ly_points(LotFrontage, SalePrice, data=train, hover=Id)
# 2 houses have an unusual LotFrontage, let's remove them
# (they are removed from all_data, but they are both in the traning set!).
all_data <- all_data[-which(all_data$LotFrontage > 300), ]
train <- train[-which(train$LotFrontage > 300), ]

# Same for the LivingArea.
figure() %>% ly_points(GrLivArea, SalePrice, data=train, hover=Id)
big_area <- subset(train, GrLivArea > 4000 & SalePrice < 200000)$Id
all_data <- all_data[-which(all_data$Id %in% big_area), ]
train <- train[-which(train$Id %in% big_area), ]


#####################
# FILL LOTFRONTAGE FOR TEST
#####################
# As we can do it by using all the training set.

# Rebuild the datasets.
train <- cbind(all_data[1:nrow(train), ], SalePrice=train$SalePrice)
test <- all_data[1:nrow(test), ]

######### DANGEROUS!!!! ##########
# Fill the train set
train <- fill_lot_frontage(train, train)


# Fill the test set
test <- fill_lot_frontage(test, train)

# Save the datasets.
write.csv(x=train, file="../data/train_cleaned.csv", row.names=F)
write.csv(x=test, file="../data/test_cleaned.csv", row.names=F)