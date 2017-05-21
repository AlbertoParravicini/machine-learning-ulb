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
library(reshape)


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

# Save the predictions done by crossvalidation
xval_pred <- data.frame(Id=c(), SalePrice=c())


######
# NN #
######


index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)

params <- expand.grid(s = c(100),
                      d = c(0.1))


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
    
    
    model<- nnet(SalePrice ~ ., data = X_tr, size=p$s, linout=T, decay=p$d, maxit=3000, relrol=2^-12)
    
    pred <- predict(model, X_val)
    cv_errors <- c(cv_errors, rmse(pred * Y_sd + Y_mean, Y_val * Y_sd + Y_mean))
    
    # Scale back the prediction.
    pred <- pred * Y_sd + Y_mean
    pred <- exp(pred) - 1
    # Round to the closest 500
    pred <- round(pred / 500) * 500
    
    pred_data_temp <- data.frame(Id=train_nn[index_test, ]$Id, SalePrice=pred)
    # Add the new predictions
    xval_pred <- rbind(xval_pred, pred_data_temp)
  }
  
  print(paste("SIZE=", p$s, "; DECAY=", p$d, "; Error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
  #############
  #############
}


xval_pred <- xval_pred %>% arrange(Id)
write.csv(x=xval_pred, file="../data/predictions/nn_xval.csv", row.names=F)

###########
# PREDICT #
###########

test_svm <- read.csv("../data/predictions/svm_fin.csv")
test_rlm <- read.csv("../data/predictions/ridge_fin_01.csv")

# Write the log-mean to a file, it is also a good average.
log_res <-  exp((log(test_rlm$SalePrice) + log(test_svm$SalePrice)) / 2)
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

model<- nnet(SalePrice ~ ., data = x_train, size=100, linout=T, decay=0.1, maxit=3000, relrol=2^-12)

pred <- predict(model, x_test)


# Scale back the prediction.
pred <- pred * Y_sd + Y_mean
pred <- exp(pred) - 1
# Round to the closest 500
pred <- round(pred / 500) * 500

pred_data <- data.frame(Id=test$Id, SalePrice=pred)

write.csv(x=pred_data, file="../data/predictions/nn_100.csv", row.names=F)

xval_pred_nn <- read.csv("../data/predictions/nn_xval.csv")
xval_pred_svm <- read.csv("../data/predictions/svm_xval.csv")
xval_pred_rlm <- read.csv("../data/predictions/ridge_xval.csv")

train_res = data.frame(RealPrice=train$SalePrice, PriceSVM=log(1+xval_pred_svm$SalePrice), PriceRidge=log(1+xval_pred_rlm$SalePrice), PriceNN=log(1+xval_pred_nn$SalePrice))

figure() %>% ly_boxplot(x=variable, y=value, data=melt(train_res)) %>% x_axis(label="Model") %>%
  y_axis(label="Log-price")

train_res_2 <- train_res - train_res$RealPrice
train_res_2$RealPrice <- NULL
train_res_2 <- train_res_2^2

train_nn <- train_nn %>% arrange(Id)
figure(legend_location = "top_left") %>% ly_points(x=train_nn$SalePrice, y = log(1 + xval_pred$SalePrice), data=train_nn, hover=Id) %>%
  ly_abline(lm(log(1 + xval_pred$SalePrice) ~ train_nn$SalePrice, data=train_nn), width=2, type=2, legend="Predicted") %>%
  ly_abline(a = 0, b = 1, width=2, type=2, color = "red", legend="Ideal")



figure() %>% ly_boxplot(X2, value, data=melt(train_res_2), outlier_glyph = NA)

summary(lm(value~X2, data=melt(train_res_2)))
