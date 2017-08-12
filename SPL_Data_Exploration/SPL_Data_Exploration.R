# Load all required packages
source("Helper_Functions.R")
list.of.packages = c("rpart", "lubridate", "outliers", "rpart.plot", "xgboost", "caret", "caretEnsemble", "randomForest", 
    "e1071", "pROC", "tidyr", "klaR", "car", "devtools", "yamldebugger", "mlbench", "Hmisc", "ggvis", "relaimpo", 
    "formatR", "data.table", "zoo", "ggplot2", "forecast", "reshape2", "pdp")
sapply(list.of.packages, load.packages)

# Loading the full dataset
FullSet = readRDS("FullSet")

# Generating an additional date variable
FullSet$NewDate = FullSet$Date

# Adding features with mean sales
FullSet$Store = as.factor(FullSet$Store)
FullSet$Date = as.Date(paste(FullSet$Date, sep = "-"), format = "%Y-%m-%d")
FullSet = separate(FullSet, Date, into = c("Year", "Month", "Day"), sep = "-")

features_avg = setNames(aggregate(FullSet$Sales, list(FullSet$Store), mean), c("Store", "AvgSalesPerStore"))
features_avg$AvgVisitsPerStore = aggregate(FullSet$Customers, list(FullSet$Store), mean)[, 2]

features_dow = setNames(aggregate(FullSet$Sales, list(FullSet$Store, FullSet$DayOfWeek), mean), c("Store", "DayOfWeek", 
    "AvgSalesPerStorePerDayOfWeek"))
features_dow$AvgVisitsPerStorePerDayOfWeek = aggregate(FullSet$Customers, list(FullSet$Store, FullSet$DayOfWeek), 
    mean)[, 3]

features_year = setNames(aggregate(FullSet$Sales, list(FullSet$Store, FullSet$Year), mean), c("Store", "Year", 
    "AvgSalesPerStorePerYear"))
features_year$AvgVisitsPerStorePerYear = aggregate(FullSet$Customers, list(FullSet$Store, FullSet$Year), mean)[, 
    3]

features_mon = setNames(aggregate(FullSet$Sales, list(FullSet$Store, FullSet$Year, FullSet$Month), mean), c("Store", 
    "Year", "Month", "AvgSalesPerStorePerMonth"))
features_mon$AvgVisitsPerStorePerMonth = aggregate(FullSet$Customers, list(FullSet$Store, FullSet$Year, FullSet$Month), 
    mean)[, 4]

# Merging new features with the dataset
FullSet = merge(FullSet, features_avg, by = "Store")
FullSet = merge(FullSet, features_dow, by = c("Store", "DayOfWeek"))
FullSet = merge(FullSet, features_year, by = c("Store", "Year"))
FullSet = merge(FullSet, features_mon, by = c("Store", "Year", "Month"))
FullSet$NewDate = as.Date(paste(FullSet$NewDate, sep = "-"), format = "%Y-%m-%d")
varlist = c("Year", "Month", "Day")
FullSet[, varlist] = lapply(FullSet[, varlist], factor)

# Plotting average sales per store by promotion
ggplot(FullSet, aes(x = Store, y = AvgSalesPerStore, color = Promo, shape = Promo)) + geom_point() + scale_color_brewer(palette = "Set2") + 
    ggtitle("Average Sales Per Store by Promo") + labs(x = "Store", y = "Average Sales Per Store") + theme_classic()
ggsave("Average_Sales_Per_Store_by_Promo.png")

# Plotting average sales per store by competition distance
FullSet.sub = subset(FullSet, Sales != 0 & !is.na(CompetitionDistance), drop = TRUE)
SalesByDist = aggregate(FullSet.sub$AvgSalesPerStore, by = list(FullSet.sub$CompetitionDistance), mean)
colnames(SalesByDist) = c("CompetitionDistance", "AvgSalesPerStore")
ggplot(SalesByDist, aes(x = CompetitionDistance, y = AvgSalesPerStore)) + geom_point() + scale_color_brewer(palette = "Set2") + 
    geom_smooth() + ggtitle("Average Sales Per Store by Competition Distance") + labs(x = "Competition Distance", 
    y = "Average Sales Per Store") + theme_bw()
ggsave("Average_Sales_by_Competition_Distance.png")

# Plotting the log of average sales
ggplot(SalesByDist, aes(x = log(CompetitionDistance), y = log(AvgSalesPerStore))) + geom_point() + scale_color_brewer(palette = "Set2") + 
    geom_smooth() + ggtitle("Log of Average Sales per Store by Log of Competition Distance") + labs(x = "Log (Competition Distance)", 
    y = "Log (Average Sales Per Store)") + theme_bw()
ggsave("Log_of_Sales_by_Competition Distance.png")

# Plotting dynamics of sales per store per month
ggplot(FullSet, aes(x = as.Date(NewDate), y = AvgSalesPerStorePerMonth)) + geom_smooth(size = 2) + ggtitle("Average Sales Per Store Per Month over Time") + 
    labs(x = "Date", y = "Average Sales Per Store Per Month") + theme_bw()
ggsave("Average_Sales_per_Store_Per_Month.png")

# Plotting dynamics of customers per store per month
ggplot(FullSet, aes(x = as.Date(NewDate), y = AvgVisitsPerStorePerMonth)) + geom_smooth(size = 2) + ggtitle("Average Customers Per Store Per Month over Time") + 
    labs(x = "Date", y = "Average Customers Per Store Per Month") + theme_bw()
ggsave("Average_Customers_per_Store_Per_Month.png")

# Loading xgb model for further plotting
xgb = readRDS("xgb")

# Calculating partial dependence
xgb.partialPlots = list()  # empty result list
imp.var.xgb = c("Open", "CompetitionDistance", "Store", "Promo", "CompetitionSinceDate", "Date", "Promo2SinceDate", 
    "StoreType", "Assortment", "Promo2", "DayOfWeek", "StoreAssortmentMatch")
for (var in imp.var.xgb) {
    message("Now calculating for variable ", var)
    xgb.partialPlots[[var]] = do.call(partial, list(xgb, pred.var = var, type = "auto", plot = FALSE))
}

# Creating the partial dependence plots and saving as pdf
par(mfrow = c(1, 2))
for (var in names(xgb.partialPlots)) {
    png(paste("PDPSales", var, ".png", sep = "_"))
    plot(x = xgb.partialPlots[[var]][, 1], y = xgb.partialPlots[[var]][, 2], type = "l", xlab = var, ylab = "Sales", 
        ylim = c(0, 8000), main = paste("Partial dependence of Sales on", var), bg = "transparent", lwd = 2)
    dev.off()
}
