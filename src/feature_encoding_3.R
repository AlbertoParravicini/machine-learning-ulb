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
library(corrplot)
library(infotheo)


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
train <- read_csv("../data/train_cleaned.csv")
test <- read_csv("../data/test_cleaned.csv")

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
# for(v in qual_vars)
# {
#   res <- train %>% dplyr::group_by_(var=v) %>%
#     dplyr::summarise(price=median(SalePrice)) %>%
#     arrange(-price)
#   p <- figure() %>% ly_bar(x = var, y=price, data=res) %>%
#     x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
#     x_axis(label=v)
#   print(p)
#   print(res)
# }
# 
# for(v in qual_vars)
# {
#   res <- train %>% dplyr::group_by_(var=v) %>%
#     dplyr::summarise(n=n()) %>%
#     arrange(-n)
#   p <- figure() %>% ly_bar(x = var, y=n, data=res, fill_color="red") %>%
#     x_range(dat=res[order(res$n, decreasing = T), ]$var) %>%
#     x_axis(label=v)
#   print(p)
#   print(res)
# }

# Add binary features for very common occurrences.
all_data$ExterQualTAGD = (all_data$ExterQual %in% c("TA", "Gd")) * 1
all_data$BsmtQualTAGD = (all_data$BsmtQual %in% c("TA", "Gd")) * 1
all_data$KitchenQualTAGD = (all_data$KitchenQual %in% c("TA", "Gd")) * 1
all_data$FireplaceNone = (all_data$FireplaceQU == "None") * 1
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
all_data$MSZoningRLRM <- (all_data$MSSubClass %in% c("RL", "RM")) * 1

# Street
table(train$Street)
all_data$Street <- (all_data$MSSubClass == "Grvl") * 1

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
all_data$BsmtPresent <- (all_data$BsmtQual != "None") * 1

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

all_data$`1stFlrSF_D` <- discretize(all_data$`1stFlrSF`, nbins = 5)$X
all_data$`2ndFlrSF_D` <- discretize(all_data$`2ndFlrSF`, nbins = 5)$X
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
