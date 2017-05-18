rm(list=ls())

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
library(corrplot)
library(infotheo)
library(dummies)
library(ggmap)

#####################
# UTILITY FUNCTIONS #
#####################

# Given a column with ordinal labels,
# map them to a column of integers given the specified dictionary.
char_to_int <- function(dict, data_to_map) {
  return(dict[data_to_map])
}


#############
# READ DATA #
#############

# Import the data
train <- read_csv("../data/train_cleaned.csv")
test <- read_csv("../data/test_cleaned.csv")
gps <- read_csv("../data/gps.csv")

all_data <- rbind(train[, -ncol(train)], test)

# Print header
print(head(train))
# Count column types
table(sapply(train, class))

gps_price <- sqldf("select train.Neighborhood, SalePrice, lat, lon from gps, train where gps.Neighborhood = train.Neighborhood")
gps_avg_price = gps_price %>% dplyr::group_by(Neighborhood, lat, lon) %>% dplyr::summarise(price=median(SalePrice), n=n())

# Print coordinates
map <- get_map(location = c(lon = mean(gps$lon), lat = mean(gps$lat)), zoom = 12, maptype = "roadmap")
map_points <- ggmap(map) +
    geom_point(aes(x = lon, y = lat, size = (3 + 30*n/max(n)), color=price), data = gps_avg_price, alpha = .8)
map_points

gmap(width=800, height = 800, lat=mean(gps_avg_price$lat), lng=mean(gps_avg_price$lon), zoom=13, map_type="roadmap", api_key = "AIzaSyDCz8Mu8rTU8cty-WAgemh0hIKrwiOHFfg") %>%
  ly_points(x=lon, y=lat, size=n/2, color=price, data=gps_avg_price, hover=Neighborhood)

# We can immediately spot the places with higher price and sales!
# The richer neighbours are close to each other.

all_data$NeighborhoodRich <- (all_data$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr")) * 1
# All the rich neighbourhoods are close to the country club!
all_data$NeighborhoodCountryClub <- (all_data$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr", "Somerst", "Blmngtn")) * 1
# Flag the neighbourhoods with a lot of sales.
all_data$NeighborhoodLarge <- (all_data$Neighborhood %in% c("CollgCr", "NAmes", "OldTown")) * 1

# Create a scale of price
c2i_dict = c('MeadowV' = 0, 'IDOTRR' = 0, 'Sawyer' = 1, 'BrDale' = 0, 'OldTown' = 1, 'Edwards' = 1, 
             'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
             'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
             'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 4, 
             'NridgHt' = 4)
all_data$NeighborhoodQualityScale <- char_to_int(c2i_dict, all_data$Neighborhood)

# Clustering on coordinates.

c4 <- kmeans(scale(gps_avg_price[, 2:3]), centers = 4)
gps_avg_price$c4 <- c4[[1]]

c8 <- kmeans(scale(gps_avg_price[, 2:3]), centers = 8)
gps_avg_price$c8 <- c8[[1]]

gmap(width=800, height = 800, lat=mean(gps_avg_price$lat), lng=mean(gps_avg_price$lon), zoom=13, map_type="roadmap", api_key = "AIzaSyDCz8Mu8rTU8cty-WAgemh0hIKrwiOHFfg") %>%
  ly_points(x=lon, y=lat, size=n/2, color=as.character(c4), data=gps_avg_price, hover=Neighborhood)
gmap(width=800, height = 800, lat=mean(gps_avg_price$lat), lng=mean(gps_avg_price$lon), zoom=13, map_type="roadmap", api_key = "AIzaSyDCz8Mu8rTU8cty-WAgemh0hIKrwiOHFfg") %>%
  ly_points(x=lon, y=lat, size=n/2, color=as.character(c8), data=gps_avg_price, hover=Neighborhood)

all_data_2 <- sqldf("select * from all_data left join gps_avg_price on gps_avg_price.Neighborhood = all_data.Neighborhood")

all_data_2 <- plyr::rename(all_data_2, c("price"="NeighborhoodAvgSale", "n"="NeighborhoodNumSales"))


########################################
# DANGEROUS:                           #
# intentionally leak from the test set #
########################################
gps_price_2 <- sqldf("select all_data.Neighborhood, lat, lon from gps, all_data where gps.Neighborhood = all_data.Neighborhood")
gps_num_sales = gps_price_2 %>% dplyr::group_by(Neighborhood, lat, lon) %>% dplyr::summarise(n=n())
all_data_2$NeighborhoodNumSalesTot <- 0
for(i in 1:nrow(all_data_2))
{
  all_data_2[i, ]$NeighborhoodNumSalesTot <- gps_num_sales[gps_num_sales$Neighborhood == all_data_2[i, ]$Neighborhood, ]$n
}

  
# Split again train/test
train_gps <- all_data_2[1:nrow(train), ]
test_gps <- all_data_2[(nrow(train)+1):nrow(all_data_2), ]

# Put back the sale price.
train_gps$SalePrice <- train$SalePrice

# Write to csv
write.csv(x=train_gps, file="../data/train_gps.csv", row.names=F)
write.csv(x=test_gps, file="../data/test_gps.csv", row.names=F)