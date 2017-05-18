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
library(e1071)
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
# SVM #
#######

# TOP "COST= 7 ; GAMMA= 0.000244140625 ; NU= 0.7 ; CV error= 0.1113  ; std dev= 0.0118"
# TOP "DEGREE= 3 ; COEF0= 1.25 ; KERNEL= polynomial ; COST= 7 ; GAMMA= 0.000244140625 ; NU= 0.7 ; CV error= 0.1094  ; std dev= 0.0098"

# obj <- tune.svm(SalePrice ~., data = x_train, gamma = 2^(-1))
# summary(obj)
# plot(obj)

index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)

gammas = 2^(-12)
costs = c(7)
type= c("nu-regression")
nus = c(0.7)
epsilons = 0.1
kernels = c("radial")
coefs0 = c(1.2)
degree = c(3)

for (g in gammas)
{
  for (c in costs)
  {
    for (k in kernels)
    {
      for (n in nus)
      {
        for (e in epsilons)
        {
          for (c0 in coefs0)
          {
            for (d in degree)
            {  
              #############
              #############
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
                
                
                model<- svm(SalePrice ~ ., data = X_tr,
                            gamma=g,
                            cost=c,
                            type="nu-regression",
                            nu=n,
                            epsilon=e,
                            kernel=k,
                            coef0=c0,
                            degree=d)
                
                pred <- predict(model, X_val)
                cv_errors <- c(cv_errors, rmse(pred * Y_sd + Y_mean, Y_val * Y_sd + Y_mean))
                
              }
              
              print(paste("DEGREE=", d, "; COEF0=", c0, "; KERNEL=", k, "; COST=", c, "; GAMMA=", g, "; NU=", n, "; CV error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
              #############
              #############
              
            }
          }
        }
      }
    }
  }
}



###########
# PREDICT #
###########

Y_mean <- mean(y_train)
Y_sd <- sd(y_train)
x_train$SalePrice <- scale(x_train$SalePrice, center = Y_mean, scale = Y_sd)

model <- svm(SalePrice ~ ., data = x_train, gamma=2^-12, cost=7, nu=0.7, type="nu-regression")

pred <- predict(model, x_test)

# Scale back the prediction.
pred <- pred * Y_sd + Y_mean
pred <- exp(pred) - 1
# Round to the closest 500
pred <- round(pred / 500) * 500

pred_data <- data.frame(Id=test$Id, SalePrice=pred)

write.csv(x=pred_data, file="../data/predictions/svm_fin.csv", row.names=F)

