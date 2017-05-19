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
library(lazy)
library(Metrics)
library(FNN)


#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_170.csv")
test <- read_csv("../data/test_170.csv")

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


########
# LAZY #
########

index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)


for (k in sizes)
{
  cv_errors <- NULL
  for (i in 1:num_folds) {
    
    index_test <- (((i-1) * fold_size + 1):(i * fold_size))  
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
    

    model<- lazy(SalePrice ~., data = X_tr)

    pred <- predict(model, X_val[1, ])$h
    
    #pred <- knn.reg(X_tr[, 1:(ncol(X_tr)-1)], X_val[, 1:(ncol(X_tr)-1)], X_tr$SalePrice, algorithm = k)$pred
    cv_errors <- c(cv_errors, rmse(pred * Y_sd + Y_mean, Y_val * Y_sd + Y_mean))
  }
  
  print(paste("K=", k, "; CV error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
}