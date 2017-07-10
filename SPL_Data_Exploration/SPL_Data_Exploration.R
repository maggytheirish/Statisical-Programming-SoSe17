# Loading necessary packages
library(data.table)
library(zoo)
library(ggplot2)
library(forecast)
library(caret)

# Loading the full dataset
train = readRDS("FullSet")

# Plotting average sales per store by promotion
ggplot(train, aes(x = Store, y = AvgSalesPerStore, color = Promo, shape = Promo)) +
    geom_point() +
    scale_color_brewer(palette="Set2") +
    ggtitle("Average Sales Per Store by Promo") +
    labs(x="Store",y="Average Sales Per Store") + theme_classic()

# Plotting average sales by competition distance
train.sub = subset(train, Sales != 0 & !is.na(CompetitionDistance), drop = TRUE)
SalesByDist = aggregate(train.sub$AvgSalesPerStore, 
               by = list(train.sub$CompetitionDistance), mean)
colnames(SalesByDist) = c("CompetitionDistance", "AvgSalesPerStore")
ggplot(SalesByDist, aes(x = CompetitionDistance, y = AvgSalesPerStore)) + 
    geom_point() + scale_color_brewer(palette="Set2") + geom_smooth() +
    ggtitle("Average Sales Per Store by Competition Distance") +
    labs(x = "Competition Distance", y = "Average Sales Per Store") +
    theme_bw()

#Plotting the log of average sales
ggplot(SalesByDist, aes(x = log(CompetitionDistance), y = log(AvgSalesPerStore))) + 
    geom_point() + scale_color_brewer(palette="Set2") + geom_smooth() +
    ggtitle("Log of Average Sales per Store by Log of Competition Distance") +
    labs(x = "Log (Competition Distance)", y = "Log (Average Sales Per Store)") + 
    theme_bw()

# Plotting dynamics of sales per store per month
ggplot(train, aes(x = as.Date(NewDate), y = AvgSalesPerStorePerMonth)) +
    geom_smooth(size = 2) +
    ggtitle("Average Sales Per Store Per Month over Time") +
    labs(x = "Date", y = "Average Sales Per Store Per Month") +
    theme_bw()
# Plotting dynamics of customers per store per month
ggplot(train, aes(x = as.Date(NewDate), y = AvgVisitsPerStorePerMonth)) +
    geom_smooth(size = 2) + 
    ggtitle("Average Customers Per Store Per Month over Time") +
    labs(x = "Date", y = "Average Customers Per Store Per Month") +
    theme_bw() 
