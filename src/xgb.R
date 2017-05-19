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
library(xgboost)
library(Metrics)


#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_fin.csv")
test <- read_csv("../data/test_fin.csv")

all_data <- rbind(train[, -ncol(train)], test)

# Print header
print(head(train))
# Count column types
table(sapply(train, class))


################
# PREPARE DATA #
################

# shuffle dataset
train <- train[sample(nrow(train)),]

x_train = train[, 2:ncol(train)]
y_train = train$SalePrice

x_test = test[, 2:ncol(test)]

num_folds = 10

# Save the predictions done by crossvalidation
xval_pred <- data.frame(Id=c(), SalePrice=c())

#######
# XGB #
#######


dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))



cv.ctrl = trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                       allowParallel=T)

xgb.grid = expand.grid(nrounds = 750,
                       eta = c(0.01,0.005,0.001),
                       max_depth = c(4,6,8),
                       colsample_bytree=c(0,1,10),
                       min_child_weight = 2,
                       subsample=c(0,0.2,0.4,0.6),
                       gamma=0.01)
set.seed(45)


xgb_params = list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE)

xgb.cv(xgb_params, dtrain, nrounds = 5000, nfold = 4, early_stopping_rounds = 500)
bst = xgb.train(xgb_params,dtrain, nrounds = 1000)



#########
y_pred.xgb = predict(bst, dtrain)
rmse(y_train, y_pred.xgb)
