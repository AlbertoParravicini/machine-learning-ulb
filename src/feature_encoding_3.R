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
