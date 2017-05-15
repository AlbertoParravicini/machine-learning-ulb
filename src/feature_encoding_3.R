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

# First, we can consider all the variables with "Cond" or "Qual" in the name,
# and no integer value.
# {ExterQual, ExterCond, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, 
#     HeatingQC, KitchenQual, FireplaceQu, GarageQual, GarageCond, PoolQC}

qual_vars = c("ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1",
              "BsmtFinType2", "HeatingQC", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")
# for(v in qual_vars)
# {
#   res <- train %>% dplyr::group_by_(var=v) %>%
#     dplyr::summarise(price=mean(SalePrice)) %>%
#     arrange(-price)
#   p <- figure() %>% ly_bar(x = var, y=price, data=res) %>%
#     x_range(dat=res[order(res$price, decreasing = T), ]$var) %>%
#     x_axis(label=v)
#   print(p)
#   print(res)
# }

# In most cases the scale is what we expect, with a few exceptions!
# We can map the values from character to numeric by using our plots.
c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "TA"=3, "Gd" = 4, "Ex" = 5)
all_data$ExterQual_n <- char_to_int(c2i_dict, all_data$ExterQual)
all_data$BsmtQual_n <- char_to_int(c2i_dict, all_data$BsmtQual)
c2i_dict = c("None"=0, "Po"=1, "Fa"=2, "Gd"=3, "TA" = 4, "Ex" = 5)
all_data$ExterCond_n <- char_to_int(c2i_dict, all_data$ExterCond)
all_data$BsmtCond_n <- char_to_int(c2i_dict, all_data$BsmtQual)


