# Load all required packages
source("Helper_Functions.R")
list.of.packages = c("rpart", "lubridate", "outliers", "rpart.plot", "xgboost", "caret", "caretEnsemble", "randomForest", 
    "e1071", "pROC", "tidyr", "klaR", "car", "devtools", "yamldebugger", "mlbench", "Hmisc", "ggvis", "relaimpo", 
    "formatR", "data.table", "zoo", "ggplot2", "forecast", "reshape2", "pdp")
sapply(list.of.packages, load.packages)

# Reading in original datasets
store = read.csv("store.csv", header = T, sep = ",")  #the dataset containing supplemental information about the stores
train = read.csv("train.csv", header = T, sep = ",")  #historical data including sales
# Merging the datasets
train = merge(store, train, by = "Store")

# Replacing a typo in CompetitionOpenSinceYear variable with a median
train$CompetitionOpenSinceYear = gsub("1900", "2010", train$CompetitionOpenSinceYear)
train$CompetitionOpenSinceYear = as.numeric(train$CompetitionOpenSinceYear)

# Replacing empty category in PromoInterval with 'missing'
train$PromoInterval = sub("^$", "missing", train$PromoInterval)

# Turning numeric variables into factor variables
varlist = c("StoreType", "Assortment", "Promo2", "DayOfWeek", "Open", "Promo", "StateHoliday", 
    "SchoolHoliday", "PromoInterval")
train[, varlist] = lapply(train[, varlist], factor)
# Formatting the date variable
train$Date <- as.Date(train$Date)

# Removing outliers
train <- subset(train, !(Open == 1 & Customers < 1 & Sales < 1))

# Creating a starting date for Promo2
train$Promo2SinceDate = as.Date(with(train, paste(Promo2SinceYear, Promo2SinceWeek, 
    1, sep = "-")), "%Y-%U-%u")

# Creating a starting date for Competition (assuming it's from the first day of the
# month)
train$CompetitionSinceDate = as.Date(paste(train$CompetitionOpenSinceYear, train$CompetitionOpenSinceMonth, 
    1, sep = "-"), format = "%Y-%m-%d")

# Replacing NAs in CompetitionSinceDate
CompetitionDateMissing = train[is.na(train$CompetitionSinceDate), ]
CompetitionDateMissing = CompetitionDateMissing[!is.na(CompetitionDateMissing$CompetitionDistance), 
    ]
CompetitionDateMissing$CompetitionSinceDate = CompetitionDateMissing$Date  # replacing NAs with the current date
train[is.na(train$CompetitionSinceDate) & !is.na(train$CompetitionDistance), "CompetitionSinceDate"] = CompetitionDateMissing$CompetitionSinceDate
train$CompetitionSinceDate = as.Date(train$CompetitionSinceDate)

# Create a new binary variable StoreAssortmentMatch
train$Assortment = as.character(train$Assortment)  # StoreType and Assortment have different levels
train$StoreAssortmentMatch = ifelse(train$StoreType == train$Assortment, 1, 0)

# Create a dummy variable for whether competition is present on a given day
train$CompetitionPresent = ifelse(train$Date < train$CompetitionSinceDate, 0, 1)
train$CompetitionPresent = as.factor(train$CompetitionPresent)
train$CompetitionPresent = factor(train$CompetitionPresent, levels = c(levels(train$CompetitionPresent), 
    "missing"))
form_na = is.na(train$CompetitionPresent)
train[form_na, "CompetitionPresent"] = "missing"

# Create a new binary variable indicating if both types of holidays take place on a
# given day
train$StateHoliday = as.character(train$StateHoliday)
train$BothHolidays = ifelse(train$StateHoliday == train$SchoolHoliday, 1, 0)

names <- c("Assortment", "StoreAssortmentMatch", "StateHoliday", "BothHolidays")
train[, names] <- lapply(train[, names], factor)

# Replace NAs in newly created CompetitionDistance, CompetitionSinceDate,
# Promo2SinceDate
train$CompetitionDistance[is.na(train$CompetitionDistance)] = mean(train$CompetitionDistance, 
    na.rm = TRUE)
train$CompetitionSinceDate[is.na(train$CompetitionSinceDate)] = mean(train$CompetitionSinceDate, 
    na.rm = TRUE)
train$Promo2SinceDate[is.na(train$Promo2SinceDate)] = mean(train$Promo2SinceDate, na.rm = TRUE)

unused.vars <- c("Promo2SinceWeek", "Promo2SinceYear", "CompetitionOpenSinceYear", "CompetitionOpenSinceMonth")
train[, unused.vars] = NULL

# Save a full dataset for data exploration use
saveRDS(train, "FullSet")

# Creating separate test set where sales need to be predicted
testdate = train$Date >= "2015-07-20"  # Selecting the last 10 days for forecasting
test = train[testdate == T, ]
train = train[testdate == F, ]  # remove test observations

# Creating train dataset
train$NewDate <- train$Date
train$NewDate = as.factor(train$NewDate)
set.seed(123)
idx = createDataPartition(train$NewDate, p = 0.05, list = F)
train_set = train[idx, ]
train_set$NewDate = NULL
test$NewDate = NULL

# Remove the variable Sales that needs to be predicted from the testing dataset
test$Sales = NULL
# Remove the variable customers related to Sales from both traiing and testing
# datasets
test$Customers = NULL
train_set$Customers = NULL

# Saving datasets
saveRDS(train_set, "train")
saveRDS(test, "test")
