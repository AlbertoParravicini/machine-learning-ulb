rm(list=ls())

library(readr)
library(purrr)
library(tidyr)
library(rbokeh)
library(plyr)
library(tidyr)
library(ggplot2)
library(moments)
library(sqldf)
library(xtable)
library(dplyr)
library(psych)
library(corrplot)
library(infotheo)
# For dummy encoding
library(caret)


#####################
# UTILITY FUNCTIONS #
#####################

# Given a column with ordinal labels,
# map them to a column of integers given the specified dictionary.
char_to_int <- function(dict, data_to_map) {
  return(dict[data_to_map])
}



#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_gps.csv")
test <- read_csv("../data/test_gps.csv")

train$Neighborhood_1 <- NULL
test$Neighborhood_1 <- NULL

all_data <- rbind(train[, -ncol(train)], test)

# Print header
print(head(train))
# Count column types
table(sapply(train, class))

###############################
# TRANSFORM ORDINAL VARIABLES #
###############################

# Some features that are represented as strings have a clear ordinal meaning!
# For instance, GarageQual = {Ex, Gd, TA, Fa, Po, NA}.
# We can associate numbers to those values, so that the order can be used by our model.

# We also consider the distributions of the values.
# In some cases, adding a boolean feature that denotes having a particular feature could be beneficial.

# First, we can consider all the variables with "Cond" or "Qual" in the name,
# and no integer value.
# {ExterQual, ExterCond, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, 
#     HeatingQC, KitchenQual, FireplaceQu, GarageQual, GarageCond, PoolQC}

qual_vars = c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
              "BsmtFinType2", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")
sales_plots = list()
for(v in qual_vars)
{
  res <- train %>% dplyr::group_by_(var=v) %>%
    dplyr::summarise(price=median(SalePrice)) %>%
    arrange(-price)
  p <- figure() %>% ly_bar(x = var, y=price, data=res) %>%
    x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
    x_axis(label=v)
  sales_plots <- c(sales_plots, list(p))
}
grid_plot(sales_plots, nrow = 4)

num_plots = list()
for(v in qual_vars)
{
  res <- train %>% dplyr::group_by_(var=v) %>%
    dplyr::summarise(n=n()) %>%
    arrange(-n)
  p <- figure() %>% ly_bar(x = var, y=n, data=res, fill_color="red") %>%
    x_range(dat=res[order(res$n, decreasing = T), ]$var) %>%
    x_axis(label=v)
  num_plots <- c(num_plots, list(p))
}
grid_plot(num_plots, nrow = 4)


# Add binary features for very common occurrences.
all_data$ExterQualTAGD = (all_data$ExterQual %in% c("TA", "Gd")) * 1
all_data$BsmtQualTAGD = (all_data$BsmtQual %in% c("TA", "Gd")) * 1
all_data$KitchenQualTAGD = (all_data$KitchenQual %in% c("TA", "Gd")) * 1
all_data$FireplaceNone = (all_data$FireplaceQu == "None") * 1
all_data$GarageQualTA = (all_data$GarageQual == "TA") * 1
all_data$GarageCondTA = (all_data$GarageCond == "TA") * 1
all_data$ExterCondTA = (all_data$ExterCond == "TA") * 1
all_data$BsmtCondTA = (all_data$BsmtCond == "TA") * 1
all_data$BsmtExposureNo = (all_data$BsmtExposure == "No") * 1
all_data$PoolNone = (all_data$PoolQC == "None") * 1


# In most cases the scale is what we expect, with a few exceptions!
# We can map the values from character to numeric by using our plots.
c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd" = 4, "Ex" = 5)
all_data$ExterQual <- char_to_int(c2i_dict, all_data$ExterQual)

all_data$BsmtQual <- char_to_int(c2i_dict, all_data$BsmtQual)
all_data$HeatingQC <- char_to_int(c2i_dict, all_data$HeatingQC)

all_data$KitchenQual <- char_to_int(c2i_dict, all_data$KitchenQual)
# No fireplace and no garage seems better than a bad fireplace or a bad garage! 
# Not by much though, so we keep the same encoding.
all_data$FireplaceQu <- char_to_int(c2i_dict, all_data$FireplaceQu)


c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "Ex"=3, "TA"=4, "Gd"=5)
all_data$GarageQual <- char_to_int(c2i_dict, all_data$GarageQual)

c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "Gd"=3, "TA" = 4, "Ex" = 5)
all_data$ExterCond <- char_to_int(c2i_dict, all_data$ExterCond)

all_data$BsmtCond <- char_to_int(c2i_dict, all_data$BsmtCond)

# GarageCond is related to price in an unexpected way.
# Higher quality is not connected to higher price.
c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "Ex"=3, "Gd" = 4, "TA" = 5)
all_data$GarageCond <- char_to_int(c2i_dict, all_data$GarageCond)

c2i_dict = c("None"=0, "Fa"=1, "TA"=2, "Gd"=3, "Ex"=4)
all_data$PoolQC <- char_to_int(c2i_dict, all_data$PoolQC)

c2i_dict = c("None"=0, "No"=1, "Mn"=2, "Av"=3, "Gd" = 4)
all_data$BsmtExposure <- char_to_int(c2i_dict, all_data$BsmtExposure)


# BsmtFinType1 and BsmtFinType2 show little difference in terms of prices.
# We can consider the BasementSize instead.
# We can actually keep both encodings and see later which is better.
res <- train %>% dplyr::group_by(var=BsmtFinType2) %>%
  dplyr::summarise(area=mean(BsmtFinSF2)) %>%
  arrange(-area)
figure() %>% ly_bar(x = var, y=area, data=res) %>%
  x_range(dat=res[order(res$area, decreasing = T), ]$var) %>%
  x_axis(label="BsmtFinType2")
res

all_data$BsmtFinType1UG = (all_data$BsmtFinType1 %in% c("Unf", "GLQ")) * 1
c2i_dict = c("None"=0, "LwQ"=1, "BLQ"=2, "Rec"=3, "ALQ" = 4, "Unf"=5, "GLQ"=6)
all_data$BsmtFinType1_2 <- char_to_int(c2i_dict, all_data$BsmtFinType1)
c2i_dict = c("None"=0, "Unf"=1, "LwQ"=2, "Rec"=3, "BLQ" = 4, "ALQ"=5, "GLQ"=6)
all_data$BsmtFinType1 <- char_to_int(c2i_dict, all_data$BsmtFinType1)

all_data$BsmtFinType2U = (all_data$BsmtFinType2 == "Unf") * 1
c2i_dict = c("None"=0, "BLQ"=1, "Rec"=2, "LwQ"=3, "Unf" = 4, "ALQ"=5, "GLQ"=6)
all_data$BsmtFinType2_2 <- char_to_int(c2i_dict, all_data$BsmtFinType2)
c2i_dict = c("None"=0, "Unf"=1, "LwQ"=2, "BLQ"=3, "Rec" = 4, "ALQ"=5, "GLQ"=6)
all_data$BsmtFinType2 <- char_to_int(c2i_dict, all_data$BsmtFinType2)

# Functional
res <- train %>% dplyr::group_by(var=Functional) %>%
  dplyr::summarise(price=mean(SalePrice)) %>%
  arrange(-price)
figure() %>% ly_bar(x = var, y=price, data=res) %>%
  x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
  x_axis(label="Functional")
res

c2i_dict = c("None"=0, "Sal"=1, "Maj2"=2, "Sev"=3, "Min2" = 4, "Min1"=5, "Maj1"=6, "Mod"=7, "Typ"=8)
all_data$Functional <- char_to_int(c2i_dict, all_data$Functional)

# Garage finish
res <- train %>% dplyr::group_by(var=GarageFinish) %>%
  dplyr::summarise(price=mean(SalePrice)) %>%
  arrange(-price)
figure() %>% ly_bar(x = var, y=price, data=res) %>%
  x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
  x_axis(label="GarageFinish")
res

c2i_dict = c("None"=0, "Unf"=1, "RFn"=2, "Fin"=3)
all_data$GarageFinish <- char_to_int(c2i_dict, all_data$GarageFinish)

# Fence
res <- train %>% dplyr::group_by(var=Fence) %>%
  dplyr::summarise(price=mean(SalePrice)) %>%
  arrange(-price)
figure() %>% ly_bar(x = var, y=price, data=res) %>%
  x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
  x_axis(label="Fence")
res

# Not having a fence is more expensive!
c2i_dict = c("MnWw"=0, "GdWo"=1, "MnPrv"=2, "GdPrv"=3, "None"=4)
all_data$Fence <- char_to_int(c2i_dict, all_data$Fence)


###############
# CORRELATION #
###############

# We can easily compute the correlation of these features now!
temp <- cbind(all_data[1:nrow(train), ], SalePrice=train$SalePrice)
correlations <- cor(as.matrix(temp[, c(qual_vars, "BsmtFinType1_2", "BsmtFinType2_2", "SalePrice")]))

corrplot(as.matrix(correlations), method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

# Nothing really surprising.
# However here garage doesn't seem that useful, and BsmtFinType2 is also useless.
# Most likely because it does have little variance.
# The alternative encoding (BsmtFinType2_2) seems sightly better, but still not very useful.

correlations <- cor(as.matrix(temp[, c(80:89, 93)]))
corrplot(as.matrix(correlations), method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)
# The binary values have little correlation, and in most cases negative correlation.
# It is a sign that the most common features are associated to cheap houses!

correlations <- cor(as.matrix(temp[, c(qual_vars, "BsmtFinType1_2", "BsmtFinType2_2", colnames(temp)[c(80:89, 93)])]))
corrplot(as.matrix(correlations), method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)


###################################
# MORE SIMPLE FEATURE ENGINEERING #
###################################

# MSSubClass
table(as.character(train$MSSubClass))
figure() %>% ly_bar(x=names(table(as.character(MSSubClass))), y=table(as.character(MSSubClass)), data=train)

all_data$MSSubClass <- as.character(all_data$MSSubClass)
all_data$MSSubClass_Common <- (all_data$MSSubClass %in% c("20", "60", "50")) * 1

# MSZoning
figure() %>% ly_bar(x=MSZoning, data=train)
all_data$MSZoningRLRM <- (all_data$MSZoning %in% c("RL", "RM")) * 1

# Street
table(train$Street)
all_data$Street <- (all_data$Street == "Grvl") * 1

# LotShape
table(train$LotShape)
all_data$LotShapeIrregular <- (all_data$LotShape %in% c("IR2", "IR3")) * 1

# LotConfig
table(train$LotConfig)
all_data$LotConfig <- if_else(all_data$LotConfig %in% c("FR2", "FR3"), "FR", all_data$LotConfig)

# LandSlope
all_data$LandSlope <- (all_data$LandSlope == "Gtl") * 1

# Condition
table(train$Condition1)
table(train$Condition2)
all_data$Condition1Abnormal <- (all_data$Condition1 != "Norm") * 1
all_data$Condition2Abnormal <- (all_data$Condition2 != "Norm") * 1
all_data$ConditionAbnormal <- all_data$Condition1Abnormal * all_data$Condition2Abnormal

# BldgType
aov(SalePrice ~ BldgType, data=temp)
all_data$BldgType <- if_else(all_data$BldgType %in% c("2fmCon", "Duplex", "Twnhs"), "Other", all_data$BldgType)
temp <- cbind(all_data[1:nrow(train), ], SalePrice=train$SalePrice)
aov(SalePrice ~ BldgType, data = temp)
# We can see how there is very little loss of variance explained.

# HouseStyle
aov(SalePrice ~ HouseStyle, data=temp)
all_data$HouseStyleUnfinished <- (all_data$HouseStyle %in% c("1.5Unf", "2.5Unf")) * 1
all_data$HouseStyleSplit <- (all_data$HouseStyle %in% c("SFoyer", "SLvl")) * 1
all_data$HouseStyle1Store <- (all_data$HouseStyle %in% c("1Story", "1.5Fin", "1.5Unf")) * 1
all_data$HouseStyle2Store <- (all_data$HouseStyle %in% c("2Story", "2.5Fin", "2.5Unf")) * 1
temp <- cbind(all_data[1:nrow(train), ], SalePrice=train$SalePrice)
aov(SalePrice ~ HouseStyleUnfinished + HouseStyleSplit + HouseStyle1Store + HouseStyle2Store, data=temp)

# RoofStyle
table(train$RoofStyle)
all_data$RoofStyle <- if_else(all_data$RoofStyle %in% c("Flat", "Gambrel", "Mansard", "Shed"), "Other", all_data$RoofStyle)

# RoofMatl
all_data$RoofMatl <- (all_data$RoofMatl != "CompShg") * 1

# Exterior1st
all_data$Exterior1st <- if_else(all_data$Exterior1st %in% names(which(table(train$Exterior1st) < 50)), "Other", all_data$Exterior1st)
# Exterior2nd
all_data$Exterior2nd <- if_else(all_data$Exterior2nd %in% names(which(table(train$Exterior2nd) < 50)), "Other", all_data$Exterior2nd)

all_data$ExteriorDiff <- (all_data$Exterior1st == all_data$Exterior2nd) * 1

# Foundation
all_data$Foundation <- if_else(all_data$Foundation %in% c("Stone", "Slab", "Wood"), "Other", all_data$Foundation)

# Basement
all_data$BsmtPresent <- (all_data$BsmtQual > 0) * 1

# Heating
all_data$Heating <- (all_data$Heating != "GasA") * 1

# CentralAir
all_data$CentralAir <- (all_data$CentralAir == "Y") * 1

# Electrical
all_data$Electrical <- if_else(all_data$Electrical %in% c("FuseF", "FuseP", "Mix"), "Other", all_data$Electrical)

# GarageType
all_data$GarageType <- if_else(all_data$GarageType %in% c("2Types", "Basment", "CarPort"), "Other", all_data$GarageType)

# Miscellaneous: the only relevant feature is the presence of a big shed.
all_data$ShedPresent <- (all_data$MiscFeature == "Shed") * 1
all_data$MiscFeature <- NULL


# MiscVal: the meaning is unclear, but we can add a binary variable to denote the presence of a positive value.
all_data$MiscValPresent <- (all_data$MiscVal > 0) * 1

# SaleType
all_data$SaleType <- if_else(all_data$SaleType %in% names(which(table(train$SaleType) < 10)), "Other", all_data$SaleType)

# SaleCondition
all_data$SaleCondition <- if_else(all_data$SaleCondition %in% names(which(table(train$SaleCondition) < 30)), "Other", all_data$SaleCondition)


# Binarize some other columns
all_data$`2ndFlrSFPresent` <- (all_data$`2ndFlrSF` == 0) * 1
all_data$MasVnrAreaPresent <- (all_data$MasVnrArea == 0) * 1
all_data$WoodDeckSFPresent <- (all_data$WoodDeckSF == 0) * 1
all_data$OpenPorchSFPresent <- (all_data$OpenPorchSF == 0) * 1
all_data$EnclosedPorchPresent <- (all_data$EnclosedPorch == 0) * 1
all_data$ScreenPorchPresent <- (all_data$ScreenPorch == 0) * 1



#############################
# DISCRETIZE SOME VARIABLES #
#############################

hist(train$LotFrontage)
all_data$LotFrontage_D <- floor(all_data$LotFrontage / 30)

hist(train$LotArea, breaks = 50)
all_data$LotAreaGiant <- (all_data$LotArea > 15000) * 1

all_data$BsmtFinSF1_D <- ceiling(all_data$BsmtFinSF1 / 400)
all_data$BsmtFinSF1_D <- all_data$BsmtFinSF1_D * all_data$BsmtPresent

all_data$BsmtFinSF2_D <- ceiling(all_data$BsmtFinSF2 / 400)
all_data$BsmtFinSF2_D <- all_data$BsmtFinSF2_D * all_data$BsmtPresent

all_data$TotalBsmtSF <- ceiling(all_data$TotalBsmtSF / 400)
all_data$TotalBsmtSF <- all_data$TotalBsmtSF * all_data$BsmtPresent

all_data$BsmtUnfSF <- ceiling(all_data$BsmtUnfSF / 400)
all_data$BsmtUnfSF <- all_data$BsmtUnfSF * all_data$BsmtPresent

all_data$NeighborhoodAvgSale_D <- discretize(all_data$NeighborhoodAvgSale, nbins = 5)$X
all_data$NeighborhoodNumSales_D <- discretize(all_data$NeighborhoodNumSales, nbins = 5)$X

all_data$`X1stFlrSF_D` <- discretize(all_data$`1stFlrSF`, nbins = 5)$X
all_data$`X2ndFlrSF_D` <- discretize(all_data$`2ndFlrSF`, nbins = 5)$X
all_data$`LowQualFinSF` <- discretize(all_data$`LowQualFinSF`, nbins = 5)$X 
all_data$`GrLivArea` <- discretize(all_data$`GrLivArea`, nbins = 5)$X 


#################
# OTHER STUFF ###
#################

# Sum the area of the house.
area_columns = c("LotFrontage", "LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF",
                "TotalBsmtSF", "1stFlrSF", "2ndFlrSF", "GrLivArea", "GarageArea", "WoodDeckSF", 
                "OpenPorchSF", "EnclosedPorch", "3SsnPorch", "ScreenPorch", "LowQualFinSF", "PoolArea")

all_data$TotalArea = as.numeric(rowSums(all_data[ , area_columns]))

all_data$InsideArea = as.numeric(all_data$`1stFlrSF` + all_data$`2ndFlrSF`)

all_data$TotalArea_D1 <- discretize(all_data$TotalArea, nbins = 20)$X 
all_data$TotalArea_D2 <- discretize(all_data$TotalArea, nbins = 10)$X 
all_data$TotalArea_D3 <- discretize(all_data$TotalArea, nbins = 5)$X 

all_data$InsideArea_D1 <- discretize(all_data$InsideArea, nbins = 20)$X 
all_data$InsideArea_D2 <- discretize(all_data$InsideArea, nbins = 10)$X 
all_data$InsideArea_D3 <- discretize(all_data$InsideArea, nbins = 5)$X 

#####################
# TEMPORAL FEATURES #
#####################

# Add season
all_data$SeasonSold <- all_data$MoSold / 4
all_data$HighSeason <- (all_data$MoSold %in% c(5, 6, 7)) * 1

# Add decade/century/millenium
all_data$BuiltDecade <- all_data$YearBuilt / 10
all_data$BuiltCentury <- all_data$YearBuilt / 100
all_data$BuiltMillenium <- (all_data$YearBuilt >= 2000) * 1

all_data$RebuiltDecade <- all_data$YearRemodAdd / 10
all_data$RebuiltCentury <- all_data$YearRemodAdd / 100
all_data$RebuiltMillenium <- (all_data$YearRemodAdd >= 2000) * 1


# Combine month + year

all_data$MonthYearSold <- all_data$MoSold + all_data$YrSold * 12

temp <- cbind(all_data[1:nrow(train), ], SalePrice=train$SalePrice)
figure() %>% ly_boxplot(x=MonthYearSold, SalePrice, data=temp)

salestime <- temp %>% dplyr::group_by(x=MonthYearSold) %>% dplyr::summarise(price=median(SalePrice))
figure(width = 800) %>% ly_lines(x, price, data=salestime, color="blue", alpha=0.5)
acf(salestime$price)

# House Id 1877 was remodelled before being built!
all_data[all_data$Id == 1877, ]$YearRemodAdd <- all_data[all_data$Id == 1877, ]$YearBuilt

# Was the house remodelled?
all_data$Remodelled <- (all_data$YearBuilt < all_data$YearRemodAdd) * 1
all_data$YearsSinceRemodel <- all_data$YearRemodAdd - all_data$YearBuilt

# Years after remodel when the house was sold.
all_data$YearsSoldAfterRemodel <- all_data$YrSold - all_data$YearRemodAdd 

# Sold in the same year as it was built.
all_data$SameYearBuiltSold <- (all_data$YearBuilt == all_data$YrSold) * 1
# Sold the same year it was rebuilt.
all_data$SameYearRebuiltSold <- (all_data$YearRemodAdd == all_data$YrSold) * 1

# Age of the house
all_data$Age <- 2010 - all_data$YearBuilt

# Age of the sale
all_data$SaleAge <- 2010 - all_data$YrSold


all_data$YearBuilt_DO <- discretize(all_data$YearBuilt, nbins = 10)$X 
all_data$YearBuilt_DD <- discretize(all_data$YearBuilt, nbins = 5)$X 

all_data$GarageYrBlt_DO <- discretize(all_data$GarageYrBlt , nbins = 10)$X 
all_data$GarageYrBlt_DD <- discretize(all_data$GarageYrBlt , nbins = 5)$X 

all_data$YearRemodAdd_DO <- discretize(all_data$YearRemodAdd, nbins = 10)$X 
all_data$YearRemodAdd_DD <- discretize(all_data$YearRemodAdd, nbins = 5)$X 

##############
# SOME PLOTS #
##############

correlations <- cor(keep(temp, is.numeric))
corrplot(correlations, method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

sales_cor <- data.frame(var=names(correlations[nrow(correlations), ]), correlation=correlations[nrow(correlations), ], row.names=NULL)

figure(width = 1200, height=800) %>% 
  ly_bar(x=var, y=correlation, data=sales_cor) %>%
  theme_axis(major_label_orientation = 45) %>%
  x_range(dat=sales_cor[order(sales_cor$correlation, decreasing = T), ]$var)



##############
# DUMMY VARS #
##############

# Turn the clusters to chracters, so they aren't normalized (they will be binarized).
all_data$c4 <- as.character(all_data$c4)
all_data$c8 <- as.character(all_data$c8)

# Turn YearBuilt_DD1 etc... to strings so that they can be binarized.
all_data$YearBuilt_DD <- as.character(all_data$YearBuilt_DD)
all_data$YearRemodAdd_DD <- as.character(all_data$YearRemodAdd_DD)
all_data$GarageYrBlt_DD <- as.character(all_data$GarageYrBlt_DD)

# Make a list of character features to be encoded.
char_columns <- names(keep(all_data, is.character))

dummy_encoder <- dummyVars(" ~ .", data=all_data[, char_columns])
all_data_dummy <- data.frame(predict(dummy_encoder, newdata = all_data))

# Replace the original string features with the encoded ones.
all_data <- cbind(all_data[, !names(all_data) %in% char_columns], all_data_dummy)

# Drop a useless column
all_data$MSSubClass150 <- NULL

# Change name to the columns that start with a number
all_data <- plyr::rename(all_data, c("1stFlrSF"="X1stFlrSF", "2ndFlrSF"="2ndFlrSF"))



##############################
# LOG SCALE NUMERIC FEATURES #
##############################


# If a feature is skewed, it can be good to log-transform it, as log(x + 1)

numeric_cols <- names(keep(all_data, is.numeric))
numeric_cols <- numeric_cols[!numeric_cols %in% c("Id", "GarageYrBlt_DD", "YearBuilt_DD", "YearRemodAdd_DD")]

skewed <- sapply(all_data[1:nrow(train), numeric_cols], skew)
skdf <- data.frame(name=numeric_cols, skewness=sapply(all_data[1:nrow(train), numeric_cols], skew), logsk=sapply(log(all_data[1:nrow(train), numeric_cols]), skew), logsk1=sapply(log(1+all_data[1:nrow(train), numeric_cols]), skew))
skdf$better <- abs(skdf$skewness) > abs(skdf$logsk1)

# Log-transform the features that have reduced skew

all_data[, as.character(subset(skdf, better)$name)] <- log(1 + all_data[, as.character(subset(skdf, better)$name)]) 


#################################
# REBUILD TRAIN, TEST, UNSCALED #
#################################

train_fin_u <- all_data[1:nrow(train), ]
test_fin_u <- all_data[(nrow(train)+1):nrow(all_data), ]

# Put back the sale price.
train_fin_u$SalePrice <- train$SalePrice

# Log-transform the price.
# No normalization, that is better to be done in cross-validation, in order not to leak information.
train_fin_u$SalePrice <- log(1 + train_fin_u$SalePrice)

# Write to csv
write.csv(x=train_fin_u, file="../data/train_fin_u.csv", row.names=F)
write.csv(x=test_fin_u, file="../data/test_fin_u.csv", row.names=F)


# Z-Score
##################################
# WARNING: SHOULD BE DONE LATER! #
##################################
########################################
# DANGEROUS:                           #
# intentionally leak from the test set #
########################################
mus <- sapply(all_data[1:nrow(all_data), numeric_cols], mean)
sigmas <- sapply(all_data[1:nrow(all_data), numeric_cols], sd)

all_data[, numeric_cols] <- scale(all_data[, numeric_cols], mus, sigmas)




###############################
# REBUILD TRAIN, TEST, SCALED #
###############################

train_fin <- all_data[1:nrow(train), ]
test_fin <- all_data[(nrow(train)+1):nrow(all_data), ]

# Put back the sale price.
train_fin$SalePrice <- train$SalePrice

# Log-transform the price.
# No normalization, that is better to be done in cross-validation, in order not to leak information.
train_fin$SalePrice <- log(1 + train_fin$SalePrice)

# Write to csv
write.csv(x=train_fin, file="../data/train_fin.csv", row.names=F)
write.csv(x=test_fin, file="../data/test_fin.csv", row.names=F)



