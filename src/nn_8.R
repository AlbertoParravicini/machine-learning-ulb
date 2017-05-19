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
library(nnet)
library(Metrics)


#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_fin.csv")
test <- read_csv("../data/test_fin.csv")

train_svm <- read_csv("../data/predictions/svm_xval.csv")
train_rlm <- read_csv("../data/predictions/ridge_xval.csv")

# Create a training set by combining the predictions.
train_nn <- data.frame(Id=train$Id, svmSalePrice=train_svm$SalePrice, rlmSalePrice=train_rlm$SalePrice, SalePrice=train$SalePrice)

# Log-scale the prices.
train_nn$svmSalePrice <- log(1 + train_nn$svmSalePrice)
train_nn$rlmSalePrice <- log(1 + train_nn$rlmSalePrice)

all_data <- rbind(train[, -ncol(train)], test)

# Print header
print(head(train_nn))
# Count column types
table(sapply(train_nn, class))


################
# PREPARE DATA #
################

# shuffle dataset
train_nn <- train_nn[sample(nrow(train_nn)),]

x_train = train_nn[, 2:ncol(train_nn)]
y_train = train_nn$SalePrice

num_folds = 10

# Print the initial values.
rmse(x_train$svmSalePrice, y_train)
rmse(x_train$rlmSalePrice, y_train)
# Just taking the mean seems better, but there is margin for improvement.
rmse((x_train$rlmSalePrice + x_train$svmSalePrice)/2, y_train)
rmse(exp((log(x_train$rlmSalePrice) + log(x_train$svmSalePrice)) / 2) , y_train)


######
# NN #
######


index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)

params <- expand.grid(g = 2^(-12),
                      c = c(7),
                      t= c("nu-regression"),
                      n = c(0.7),
                      e = c(0.1),
                      k = c("radial"),
                      c0 = c(1.2),
                      d = c(5))


for (i in 1:nrow(params))
{
  #############
  #############
  p <- params[i, ]
  
  cv_errors <- NULL
  for (i in 1:num_folds) {
    
    index_test <- (((i-1) * fold_size + 1):(i * fold_size))  
    if (i==num_folds)
    {
      index_test = c(index_test, ((i * fold_size)+1):nrow(x_train))
    }
    index_train <- setdiff(index, index_test)
    
    X_tr <- x_train[index_train, ]
    Y_tr <- y_train[index_train]  
    X_val <- x_train[index_test, ]  
    Y_val <- y_train[index_test] 
    
    # Scale x
    x_mean <- sapply(X_tr, mean)
    x_sd <- sapply(X_tr, sd)
    var_zero <- x_sd == 0
    X_tr <- data.frame(scale(X_tr, center = x_mean, scale = x_sd))
    X_tr[, var_zero] <- NULL
    X_val <- data.frame(scale(X_val, center = x_mean, scale = x_sd))
    X_val[, var_zero] <- NULL
    
    
    # Scale the output.
    Y_mean <- mean(Y_tr)
    Y_sd <- sd(Y_tr)
    Y_tr <- scale(Y_tr, center = Y_mean, scale = Y_sd)
    Y_val <- scale(Y_val, center = Y_mean, scale = Y_sd)
    
    
    model<- nnet(SalePrice ~ ., data = X_tr, size=100, linout=T, decay=0.1, maxit=3000, relrol=2^-12)
    
    pred <- predict(model, X_val)
    cv_errors <- c(cv_errors, rmse(pred * Y_sd + Y_mean, Y_val * Y_sd + Y_mean))
  }
  
  print(paste("Error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
  #############
  #############
}

###########
# PREDICT #
###########

test_svm <- read.csv("../data/predictions/svm_fin.csv")
test_rlm <- read.csv("../data/predictions/ridge_fin_01.csv")
test_ela <- read.csv("../data/predictions/elastic_fin.csv")

# Write the log-mean to a file, it is also a good average.
log_res <-  exp((log(test_rlm$SalePrice) + log(test_svm$SalePrice) + log(test_ela$SalePrice)) / 3)
# Round to the closest 500
log_res <- round(log_res / 500) * 500
write.csv(x=data.frame(Id=test$Id, SalePrice=log_res), file="../data/predictions/log_res.csv", row.names = F)

# Write the mean to a file, maybe it's better.
mean_res <- (test_rlm$SalePrice + test_svm$SalePrice) / 2
# Round to the closest 500
mean_res <- round(mean_res / 500) * 500
write.csv(x=data.frame(Id=test$Id, SalePrice=mean_res), file="../data/predictions/mean_res.csv", row.names = F)


# Create a training set by combining the predictions.
x_test <- data.frame(svmSalePrice=test_svm$SalePrice, rlmSalePrice=test_rlm$SalePrice)

# Log-scale the prices.
x_test$svmSalePrice <- log(1 + x_test$svmSalePrice)
x_test$rlmSalePrice <- log(1 + x_test$rlmSalePrice)

# Scale x_train and x_test
Y_mean <- mean(x_train$SalePrice)
Y_sd <- sd(x_train$SalePrice)

x_mean <- sapply(x_train, mean)
x_sd <- sapply(x_train, sd)
var_zero <- x_sd == 0
x_train <- data.frame(scale(x_train, center = x_mean, scale = x_sd))
x_test <- data.frame(scale(x_test, center = x_mean[1:(ncol(x_train)-1)], scale = x_sd[1:(ncol(x_train)-1)]))
x_train[, var_zero] <- NULL
x_test[, var_zero] <- NULL

model<- nnet(SalePrice ~ ., data = x_train, size=64, linout=T, decay=1, maxit=300)

pred <- predict(model, x_test)


# Scale back the prediction.
pred <- pred * Y_sd + Y_mean
pred <- exp(pred) - 1
# Round to the closest 500
pred <- round(pred / 500) * 500

pred_data <- data.frame(Id=test$Id, SalePrice=pred)

write.csv(x=pred_data, file="../data/predictions/nn_64.csv", row.names=F)









