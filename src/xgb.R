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

x_train = train[, 2:(ncol(train)-1)]
y_train = train$SalePrice
Y_mean <- mean(y_train)
Y_sd <- sd(y_train)

x_test = test[, 2:ncol(test)]


#######
# XGB #
#######

# x_mean <- sapply(x_train, mean)
# x_sd <- sapply(x_train, sd)
# x_train <- scale(x_train, x_mean, x_sd)
# y_train = (y_train - Y_mean) / Y_sd
# x_test <- scale(x_test, x_mean, x_sd)



dtrain = xgb.DMatrix(as.matrix(x_train), label = y_train)
dtest = xgb.DMatrix(as.matrix(x_test))



# cv.ctrl = trainControl(method = "repeatedcv", repeats=1, number=4, 
#                        allowParallel=T)
# 
# xgb.grid = expand.grid(nrounds = 750,
#                        eta = c(0.01,0.005,0.001),
#                        max_depth = c(4,6,8),
#                        colsample_bytree=c(0,1,10),
#                        min_child_weight = 2,
#                        subsample=c(0,0.2,0.4,0.6),
#                        gamma=0.01)
# set.seed(45)
# 
# 
# xgb_params = list(
#   booster = 'gbtree',
#   objective = 'reg:linear',
#   eval_metric="rmse",
#   colsample_bytree=1,
#   eta=0.005,
#   max_depth=4,
#   min_child_weight=3,
#   alpha=0.3,
#   lambda=0.4,
#   gamma=0.01, # less overfit
#   subsample=0.6,
#   seed=5,
#   silent=TRUE)
# 
# xgb.cv(xgb_params, dtrain, nrounds = 5000, nfold = 4, early_stopping_rounds = 500)
# model = xgb.train(xgb_params, dtrain, nrounds = 5000)

best_param = list()
best_seednumber = 1234
best_rmse = Inf
best_rmse_index = 0

for (iter in 1:10) {
  param <- list(objective = "reg:linear",
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .001, .01),
                gamma = runif(1, 0.0, 0.05), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 5000
  cv.nfold = 10
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stop_rounds=500, stratified = T, maximize = F)
  
  min_rmse = min(mdcv$evaluation_log[, "test_rmse_mean"])
  min_rmse_index = which.min(as.numeric(unlist(mdcv$evaluation_log[, "test_rmse_mean"])))
  
  if (min_rmse < best_rmse) {
    best_rmse = min_rmse
    best_rmse_index = min_rmse_index
    best_seednumber = seed.number
    best_param = param
  }
}



#########
# y_pred.xgb = predict(bst, dtrain)
# rmse(y_train, y_pred.xgb)


nround = best_rmse_index
set.seed(best_seednumber)
model <- xgb.train(data=dtrain, params=best_param, nrounds=nround, nthread=6)

pred <- predict(model, dtest)

# pred <- pred * Y_sd + Y_mean
pred <- exp(pred) - 1
# Round to the closest 500
pred <- round(pred / 500) * 500

pred_data <- data.frame(Id=test$Id, SalePrice=pred)

write.csv(x=pred_data, file="../data/predictions/xgb_fin.csv", row.names=F)
