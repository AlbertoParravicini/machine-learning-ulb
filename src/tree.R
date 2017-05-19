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
library(rpart)
library(Metrics)


#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_fin_u.csv")
test <- read_csv("../data/test_fin_u.csv")

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
# TREE #
########

index = sample(1:nrow(x_train))
fold_size <- floor(nrow(x_train) / num_folds)

minsplits = c(5)
cps = 2^(-9)

num_trees = 40

for (m in minsplits)
{
  for (c in cps)
  {
    #######
    #######
    cv_errors <- numeric(num_folds)
    for (i in 1:num_folds) {
      
      index_test <- (((i-1) * fold_size + 1):(i * fold_size))  
      index_train <- setdiff(index, index_test)
      
      X_val <- x_train[index_test, ]  
      Y_val <- y_train[index_test] 
      
      Y_scores <- matrix(0, nrow=nrow(X_val), ncol=num_trees)
      
      # Build a forest.
      for (r in 1:num_trees) {
        index_train_resample <- sample(index_train, rep=T)
        X_tr <- x_train[index_train_resample, ]
        Y_tr <- y_train[index_train_resample]                          
        
        model<- rpart(SalePrice ~., data = X_tr, method = "anova", minsplit=m, cp=c)
        
        Y_scores[, r] <- predict(model, X_val)
      }
      Y_scores_tot <- apply(Y_scores, 1, mean)
      cv_errors[i] <- rmse(Y_scores_tot, Y_val)
      print(paste(i, ")", "ERROR=", cv_errors[i]))
    }
    
    print(paste("CP=", c, "; MINSPLIT=", m, "; CV error=", round(mean(cv_errors), digits=4), " ; std dev=", round(sd(cv_errors), digits=4)))
    #######
    #######
  }
}


###########
# PREDICT #
###########

# Y_mean <- mean(y_train)
# Y_sd <- sd(y_train)
# x_train$SalePrice <- scale(x_train$SalePrice, center = Y_mean, scale = Y_sd)
# 
# model<- linearRidge(SalePrice ~., data = x_train, scaling = "none", lambda = 20)
# 
# pred <- predict(model, x_test)
# 
# 
# # Scale back the prediction.
# pred <- pred * Y_sd + Y_mean
# pred <- exp(pred) - 1
# # Round to the closest 500
# pred <- round(pred / 500) * 500
# 
# pred_data <- data.frame(Id=test$Id, SalePrice=pred)
# 
# write.csv(x=pred_data, file="../data/predictions/tree_fin.csv", row.names=F)


