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
library(moments)
library(corrplot)
library(tabplot)
library(ICC)


# Import the data
train <- read_csv("~/machine-learning-ulb/data/train.csv")
test <- read_csv("~/machine-learning-ulb/data/test.csv")

# Print header
print(head(train))
# Count column types
table(sapply(train, class))


#####################
# INSPECTING TARGET #
#####################

p <- figure() %>%
    ly_hist(SalePrice, data=train, breaks=30, freq=F, legend="Sale price histogram") %>%
    ly_density(rnorm(n=800000, mean=mean(train$SalePrice), sd=sd(train$SalePrice)), line_dash=1, width=2, alpha=0.4, color="#F08080", legend="Normal density")
p
# The sale price is clearly not normal! 
# It is skewed to the right, and leptokurtic.
summary(train$SalePrice)
shapiro.test(train$SalePrice)
skewness(train$SalePrice)
kurtosis(train$SalePrice)

# Instead, we can trasform it as log(1 + x) (+1 so no problems with values = 0)
log_price = log(1 + train$SalePrice)

p <- figure() %>%
  ly_hist(log_price, breaks=30, freq=F, legend="Sale price histogram") %>%
  ly_density(rnorm(n=800000, mean=mean(log_price), sd=sd(log_price)), line_dash=1, width=2, alpha=0.4, color="#F08080", legend="Normal density")
p

summary(log_price)
shapiro.test(log_price)
skewness(log_price)
kurtosis(log_price)

# Now the data resemble a gaussian, but they are still not normal!
# Still, it should be better than before.


#############################
# INSPECTING CATEGORICAL DISTRIBUTIONS 
#############################

train_char <- train[, sapply(train, class)=="character"]
test_char <- test[, sapply(test, class)=="character"]

train_char$Type <- "train"
test_char$Type <- "test"

all_char <- rbind(train_char, test_char)

lapply(train_char, table)

# Compare the distributions of the train and test set.

# 2 different plots, just to have bigger plots
names <- colnames(all_char)
all_char %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[1:20]) %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

all_char %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[21:length(names)]) %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# we can see how many features have a very common value, and few occurrencies of other values.
# Still, the rare occurrencies of the other values could be good predictors of the final price.
# In some cases, however, we can probably ignore the feature, or simplify.
# In many cases it's good to aggregate uncommon features.

# Train and test sets have similar distributions.

# List of character columns with a single very common value
maxperc <- sapply(train_char, function(x) max(table(x)) / length(x))
common <- colnames(train_char)[maxperc > 0.9]
for(c in common)
{
  print(paste(c, ": ", names(which.max(table(train_char[, c]))), ", ", maxperc[c]))
}

#################################
# INSPECTING NUMERICAL FEATURES #
#################################

train_num <- keep(train, is.numeric)
sapply(train_num, summary)

# No negative values -> GOOD
# A bunch of NA's -> NOT SO GOOD
#   LotFrontage NA's are probably == 0;
#   Are MasVnrArea NA's == 0 ?
train[is.na(train$MasVnrArea), "MasVnrType"]
#   They are NA too! 
#   We could remove those observations(just 8), or those variables if not very useful.
#   Another option is just to replace with "none, 0", which is the most common pair.
#   It will reduce the variance, but for just 8 rows it's not a big deal.

# Some numerical features are ordinal factors (year) -> some models might work better with a conversion.

# What about the houses with NA in garage?
train[is.na(train$GarageYrBlt), c("GarageType", "GarageFinish", "GarageCars", "GarageArea", "GarageQual")]
# NA and 0 in garage-related columns are related to the absence of garages.
# We can keep the 0 in numeric features, and replace NA with "none" in categorical.
# Years are more problematic, if treated as numeric: 
# we could convert it to categorical, or just remove the feature, if superfluous.

# What about the test set?
test_num <- keep(test, is.numeric)
sapply(test_num, summary)
# A couple of ugly things:
# There are more NA's
# There is 1 row with NA's in features relative to the basement. 
# This house doesn't have a basement, so we can replace the NA's with 0
test[is.na(test$BsmtFinSF1), "BsmtFinType1"]
test[is.na(test$BsmtFinSF2), "BsmtFinType2"]
# There is a second row with dirty data, and no basement.
basement_row <- test[is.na(test$BsmtFullBath), ]


# Distributions
train_num$Type <- "train"
train_num$SalePrice <- NULL
test_num$Type <- "test"

all_num <- rbind(train_num, test_num)
all_num$Id <- NULL

# 4 different plots, just to have bigger plots
names <- colnames(all_num)
all_num %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[1:10] & Type == "train") %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge", alpha=0.5) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

all_num %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[11:20]) %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge", alpha=0.5) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

all_num %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[21:30]) %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge", alpha=0.5) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

all_num %>%
  gather(key, value, -Type) %>% 
  filter(key %in% names[30:ncol(all_num)]) %>%
  ggplot(aes(value, fill=Type)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(stat="count", position="dodge", alpha=0.5) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Notes: 
#   Train and test show similar distributions, also the same peaks
#   The test set has a garage built in 2200! We can replace it with the year the house was built.
#   Some Values have non-gaussian distributions, we can fix those too.
#   A lot of features have strong peaks in 0

#############################
# INSPECTING MISSING VALUES #
#############################

# Count NA per column
na_train <- data.frame(col=colnames(train),
                       col_type=sapply(train, class),
                       na_sum=apply(X = train, MARGIN = 2, FUN=function(x) sum(is.na(x))),
                       na_perc=apply(X = train, MARGIN = 2, FUN=function(x) sum(is.na(x))/length(x)), row.names = NULL)
na_train <- na_train[order(na_train$na_perc, decreasing = T), ]
# A few columns have really high NA count. Probably the NA has a meaning, we can replace it with a default value.
# PoolQC is NA if no pool is present.
# MiscFeature is NA if no special feature is present.
# As the percentage of missing is high, it's probably better to use a binary variable (e.g. PoolPresent = Y/N)
# We can do this for PoolQC, MiscFeature, Alley and Fence

# FireplaceQu is better kept as is, but we can still add a default value for no fireplace.

# Lot frontage has NA value instead of 0, as the min value is 21.

# Other missing data are connected with each other.
# We'll see what to do with them later, depending on how useful they are and depending on the test set.

na_test <- data.frame(col=colnames(test),
                       col_type=sapply(test, class),
                       na_sum=apply(X = test, MARGIN = 2, FUN=function(x) sum(is.na(x))),
                       na_perc=apply(X = test, MARGIN = 2, FUN=function(x) sum(is.na(x))/length(x)), row.names = NULL)
na_test <- na_test[order(na_test$na_perc, decreasing = T), ]

# The test set has a similar NA distribution, but there are more columns with 1-2 values missing. 
# Models should be able to handle that anyway, however. If not, we'll think what to do at a later stage.



###########################
# CORRELATION WITH TARGET #
###########################

# First, inspect the correlation of numerical features with the target
train_num <- keep(train, is.numeric)
train_num$Id <- NULL
correlations <- cor(na.omit(train_num))
sales_cor <- data.frame(var=names(correlations[37, ]), correlation=correlations[37, ], row.names=NULL)

figure(width = 1200, height=800) %>% 
  ly_bar(x=var, y=correlation, data=sales_cor) %>%
  theme_axis(major_label_orientation = 45) %>%
  x_range(dat=sales_cor[order(sales_cor$correlation, decreasing = T), ]$var)

# A few numerical features have a strong positive correlation:
# Overall quality, living area, basement area...
# Garage Cars and Garage Area are strongly dependent -> Using highly correlated features is bad!

# Other features, such as OverallCond, are surprisingly not very correlated!
hist(train$OverallCond)

indices <- abs(correlations[37, ]) > 0.2
corrplot(correlations[indices, indices], method="square")

# We can spot some patterns: 
# All the features related to garages are correlated,
# and also GarageYrBlt is correlated to YearBuilt.
# The unfinished square meters of basement are negatively correlated to the meters of finished basement.
train[1:20, c("BsmtUnfSF", "BsmtFinSF1", "BsmtFinSF2", "TotalBsmtSF")]
# "TotalBsmtSF" is the sum of the other 3 features.

indices <- abs(correlations[37, ]) > 0.6
tableplot(train_num[, indices], cex=1.8, sortCol = "SalePrice")


# Plot some scatter plots for the most correlated numerical features
figure(legend_location = NULL) %>%
  ly_points(train_num$OverallQual, train_num$SalePrice, color=train_num$OverallQual) %>%
  ly_abline(lm(SalePrice ~ OverallQual, data=train_num), width=2, type=2)

# There are 2 outliers on the right, maybe they can be removed.
figure(legend_location = NULL) %>%
  ly_points(train_num$GrLivArea, train_num$SalePrice, color=train_num$GrLivArea) %>%
  ly_abline(lm(SalePrice ~ GrLivArea, data=train_num), width=2, type=2)

# 4 cars garages have lower sale price?
# It seems that there are a few outliers, but they aren't the same houses as before!
figure(legend_location = NULL) %>%
  ly_points(train_num$GarageCars, train_num$SalePrice, color=train_num$GarageCars) %>%
  ly_abline(lm(SalePrice ~ GarageCars, data=train_num), width=2, type=2)
figure(legend_location = NULL) %>%
  ly_points(train_num$GarageArea, train_num$SalePrice, color=train_num$GarageArea) %>%
  ly_abline(lm(SalePrice ~ GarageArea, data=train_num), width=2, type=2)
figure(legend_location = NULL) %>%
  ly_boxplot(x=GarageCars, y=SalePrice, group=GarageCars, data=train_num)

# All houses until the '50 have been remodeled, and they have lower sale price.
train_num$rebuilt <- if_else(train_num$YearBuilt < train_num$YearRemodAdd, "true", "false")
figure() %>%
  ly_points(YearBuilt, SalePrice, color=rebuilt, data=train_num) %>%
  ly_abline(lm(SalePrice ~ YearBuilt, data=train_num), width=2, type=2)
figure(legend_location = NULL, width=1200) %>%
  ly_boxplot(x=YearBuilt, y=SalePrice, color=YearBuilt, data=train_num, outlier_glyph = NA) %>%
  theme_axis(major_label_orientation = 45) 

# Among recent houses, it seems that rebuilt houses have in general higher price.
figure(legend_location = NULL) %>%
  ly_boxplot(y=SalePrice, x=rebuilt, data=subset(train_num, YearBuilt > 1950))


# Some numeric features can be log-transformed

# Above-ground living area size is definitely not normal!
figure() %>%
  ly_hist(GrLivArea, data=train_num, breaks = 20, freq=F, legend="Living area") %>%
  ly_density(rnorm(n=800000, mean=mean(GrLivArea), sd=sd(GrLivArea)), line_dash=1, width=3, alpha=0.4, color="#F08080", legend="Normal density", data=train_num)
skewness(train_num$GrLivArea)
kurtosis(train_num$GrLivArea)

log_area <- log(1 + train_num$GrLivArea)
figure() %>%
  ly_hist(log_area, data=train_num, breaks = 20, freq=F, legend="Living area") %>%
  ly_density(rnorm(n=800000, mean=mean(log_area), sd=sd(log_area)), line_dash=1, width=3, alpha=0.4, color="#F08080", legend="Normal density")
skewness(log_area)
kurtosis(log_area)
# Still not normal, but skewness and kurtosis are better now.
shapiro.test(log_area)


# Above-ground living area size is definitely not normal!
figure() %>%
  ly_hist(TotalBsmtSF, data=train_num, breaks = 30, freq=F, legend="Living area") %>%
  ly_density(rnorm(n=800000, mean=mean(TotalBsmtSF), sd=sd(TotalBsmtSF)), line_dash=1, width=3, alpha=0.4, color="#F08080", legend="Normal density", data=train_num)
skewness(train_num$TotalBsmtSF)
kurtosis(train_num$TotalBsmtSF)

log_area <- log(1 + train_num$TotalBsmtSF)
figure() %>%
  ly_hist(log_area, data=train_num, breaks = 30, freq=F, legend="Living area") %>%
  ly_density(rnorm(n=800000, mean=mean(log_area), sd=sd(log_area)), line_dash=1, width=3, alpha=0.4, color="#F08080", legend="Normal density")
skewness(log_area)
kurtosis(log_area)
# In this case, log transform makes thing worse! 
shapiro.test(log_area)



##################
# ANOVA ##########
##################
train_char$SalePrice <- train$SalePrice
train_char <- train_char %>% mutate_if(is.character, as.factor)
res_aov <- aov(SalePrice ~ ., data=train_char[, c(1:2, 4:38, 42:43, 45)])

# Most important features: neighbourhood, MSZoning, LotShape, BldgType
figure() %>%
  ly_boxplot(x=Neighborhood, y=SalePrice, data=train) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 

figure() %>%
  ly_boxplot(x=MSZoning, y=SalePrice, data=train) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 

figure() %>%
  ly_boxplot(x=LotShape, y=SalePrice, data=train) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 

figure() %>%
  ly_boxplot(x=BldgType, y=SalePrice, data=train) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 

train_nz = train %>% dplyr::group_by(Neighborhood, MSZoning) %>% dplyr::summarize(count=n(), sale=mean(SalePrice))

figure(legend_location = "bottom_left") %>%
  ly_points(x=Neighborhood, y=MSZoning, size=(10+80*count/max(count)), color=sale, data=train_nz) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 
# Lots of sales in NAmes and CollgCr.
# Crawfor, StoneBr and NridgHt have few sales, but average high price.

res_aov_e <- res_aov$coefficients[order(res_aov$coefficients, decreasing = T)]

res_aov_e <- res_aov_e / max(res_aov_e)

res_aov_ef <- res_aov_e[abs(res_aov_e) > 0.2]
res_aov_ef <- data.frame(name=names(res_aov_ef), val=res_aov_ef)

figure(width=800) %>%
  ly_bar(x = name, y=val, data=res_aov_ef) %>%
  x_range(dat=res_aov_ef[order(res_aov_ef$val, decreasing = T), ]$name) %>%
  theme_axis(major_label_orientation = 45, axis_label_text_font_size = 20) 


# Roof material seems to be important!
figure() %>%
  ly_boxplot(x=RoofMatl, y=SalePrice, data=train)
# Sale condition: partial is associated to new houses, so it's more expensive (OR NOT?!?!?)
figure() %>%
  ly_boxplot(x=SaleCondition, y=SalePrice, data=train)

# Final tableplot
features <- c("SalePrice", "Neighborhood", "MSZoning", "YearBuilt", "GarageCars", "GrLivArea", "OverallQual")
tableplot(train[, features], cex=1.8, sortCol = "SalePrice")
# High price houses are in the same place (the green one)
# Low price houses are in the brown-ish one.
# RM are cheaper.
# Numerical values works as expected.

