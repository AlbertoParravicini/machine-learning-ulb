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
library(ridge)
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

# Which features are the most correlated to the output? Keep the top 10 (not counting sale price itself)
correlations <- names(x_train)[order(cor(x_train, y_train), decreasing = T)[2:11]]


#########
# RIDGE #
#########

   
index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)

lambdas = c(20)

for (l in lambdas)
{
  cv_errors <- NULL
  
  for (i in 1:num_folds)
  {
    index_test <- (((i-1) * fold_size + 1):(i * fold_size))  
    # Make sure that all the examples are tested.
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
    # Remove rows with variance = 0
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
    
    
    model<- linearRidge(SalePrice ~., data = X_tr, scaling = "none", lambda = l)
    
    pred <- predict(model, X_val)
    cv_errors <- c(cv_errors, rmse(pred * Y_sd + Y_mean, Y_val * Y_sd + Y_mean))
    
    # Scale back the predictions.
    pred <- pred * Y_sd + Y_mean
    pred <- exp(pred) - 1
    # Round to the closest 500
    pred <- round(pred / 500) * 500
    
    pred_data_temp <- data.frame(Id=train[index_test, ]$Id, SalePrice=pred)
    # Add the new predictions
    xval_pred <- rbind(xval_pred, pred_data_temp)
  }
    
  print(paste("LAMBDA=", l, "; CV error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
}
  
  


xval_pred <- xval_pred %>% arrange(Id)
write.csv(x=xval_pred, file="../data/predictions/ridge_xval.csv", row.names=F)

###########
# PREDICT #
###########

Y_mean <- mean(y_train)
Y_sd <- sd(y_train)
x_train$SalePrice <- scale(x_train$SalePrice, center = Y_mean, scale = Y_sd)

model<- linearRidge(SalePrice ~., data = x_train, scaling = "none", lambda = 20)

pred <- predict(model, x_test)


# Scale back the prediction.
pred <- pred * Y_sd + Y_mean
pred <- exp(pred) - 1
# Round to the closest 500
pred <- round(pred / 500) * 500

pred_data <- data.frame(Id=test$Id, SalePrice=pred)

write.csv(x=pred_data, file="../data/predictions/ridge_fin_01.csv", row.names=F)

coef_df <- data.frame(x=rownames(model$coef), y=model$coef)
figure(width=1200, height=800) %>% ly_bar(x, y, data=coef_df) %>%
  theme_axis(major_label_orientation = 45) %>%
  x_range(dat=coef_df[order(coef_df$y, decreasing = T), ]$x)


figure(width=1200, height=800) %>% ly_bar(x, y, data=subset(coef_df, abs(y) > 0.04)) %>%
  theme_axis(major_label_orientation = 45) %>%
  x_range(dat=subset(coef_df, abs(y) > 0.04)[order(subset(coef_df, abs(y) > 0.04)$y, decreasing = T), ]$x)

train <- train %>% arrange(Id)
figure(legend_location = "top_left") %>% ly_points(x=train$SalePrice, y = log(1 + xval_pred$SalePrice), data=train, hover=Id) %>%
  ly_abline(lm(log(1 + xval_pred$SalePrice) ~ train$SalePrice, data=train), width=2, type=2, legend="Predicted") %>%
  ly_abline(a = 0, b = 1, width=2, type=2, color = "red", legend="Ideal")


