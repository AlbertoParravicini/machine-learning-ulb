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


#######
# PCA #
#######

pca_res <- prcomp(train[, -c(1, ncol(train))], center=T, scale=T)

figure() %>% ly_bar(x=as.factor(1:length(pca_res$sdev)), y=pca_res$sdev) %>%
  x_range(dat=as.factor((1:length(pca_res$sdev))[order(pca_res$sdev, decreasing = T)]))

# Features after 228 have no information!
# Using about 205 features will give 99% of the variance.
# Using 170 features will give 95% of the variance.
cumsum(pca_res$sdev / sum(pca_res$sdev))

# We can store both models, and see later which one to use.

#######
# 170 #
#######
train_170 <- scale(train[, -c(1, ncol(train))], pca_res$center, pca_res$scale) %*% pca_res$rotation[, 1:170]
test_170 <- scale(test[, -1], pca_res$center, pca_res$scale) %*% pca_res$rotation[, 1:170]

train_170 <- data.frame(train_170)
train_170 <- cbind(Id=train$Id, train_170)
train_170$SalePrice <- train$SalePrice

test_170 <- data.frame(test_170)
test_170 <- cbind(Id=test$Id, test_170)
                  
write.csv(x=train_170, file="../data/train_170.csv", row.names=F)
write.csv(x=test_170, file="../data/test_170.csv", row.names=F)

#######
# 205 #
#######
train_205 <- scale(train[, -c(1, ncol(train))], pca_res$center, pca_res$scale) %*% pca_res$rotation[, 1:205]
test_205 <- scale(test[, -1], pca_res$center, pca_res$scale) %*% pca_res$rotation[, 1:205]

train_205 <- data.frame(train_205)
train_205 <- cbind(Id=train$Id, train_205)
train_205$SalePrice <- train$SalePrice

test_205 <- data.frame(test_205)
test_205 <- cbind(Id=test$Id, test_205)

write.csv(x=train_205, file="../data/train_205.csv", row.names=F)
write.csv(x=test_205, file="../data/test_205.csv", row.names=F)