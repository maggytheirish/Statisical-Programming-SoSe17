#################################################################### 

          ### Data Cleaning and Feature Engineering ###

#################################################################### 
# Function to load the packages
load.packages = function(p) {
    
    for (i in seq_along(p)) {
        if (!require(p[i], character.only = TRUE)) {
            install.packages(p[i])
        }
        library(p[i], character.only = TRUE)
    }
    
}

# Function to load the datasets
load.multiple.files = function(path, pattern) {
    
    all.csv.files = list()
    list.filenames = list.files(path, pattern = glob2rx(pattern))
    
    for (i in 1:length(list.filenames)) {
        all.csv.files[[i]] = read.csv(paste(path, list.filenames[i], sep = "/"), sep = ",")
    }
    
    names(all.csv.files) = list.filenames
    
    return(all.csv.files)
}

# Load all required packages
list.of.packages = c("rpart", "lubridate", "outliers", "rpart.plot", "xgboost", "caret", "caretEnsemble", "randomForest", 
    "e1071", "pROC", "tidyr", "klaR", "car", "devtools", "yamldebugger", "mlbench", "Hmisc", "ggvis", "relaimpo", 
    "formatR", "data.table", "zoo", "ggplot2", "forecast", "reshape2", "pdp")

sapply(list.of.packages, load.packages)

# Loading the datasets
data.list = load.multiple.files(getwd(), "*csv*")

store = data.list$store.csv  #the dataset containing supplemental information about 
# the stores
train = data.list$train.csv  #historical data including sales

# Merging the datasets
train = merge(store, train, by = "Store")

### Cleaning

# Replacing a typo in CompetitionOpenSinceYear variable with a median
train$CompetitionOpenSinceYear = gsub("1900", "2010", train$CompetitionOpenSinceYear)
train$CompetitionOpenSinceYear = as.numeric(train$CompetitionOpenSinceYear)

# Replacing empty category in PromoInterval with 'missing'
train$PromoInterval = sub("^$", "missing", train$PromoInterval)

# Removing outliers
train = subset(train, !(Open == 1 & Customers < 1 & Sales < 1))

### Feature engineering

# Creating a starting date for Promo2
train$Promo2SinceDate = as.Date(with(train, paste(Promo2SinceYear, Promo2SinceWeek, 1, sep = "-")), "%Y-%U-%u")

# Creating a starting date for Competition (assuming it's from the first day of the month)
train$CompetitionSinceDate = as.Date(paste(train$CompetitionOpenSinceYear, train$CompetitionOpenSinceMonth, 
    1, sep = "-"), format = "%Y-%m-%d")

# Replacing NAs in CompetitionSinceDate
CompetitionDateMissing = is.na(train$CompetitionSinceDate) == T & !is.na(train$CompetitionDistance) == T
train$CompetitionSinceDate[CompetitionDateMissing == T] = train$Date[CompetitionDateMissing == T]

# Create a new binary variable StoreAssortmentMatch
train$Assortment = as.character(train$Assortment)  # StoreType and Assortment have different levels
train$StoreAssortmentMatch = ifelse(train$StoreType == train$Assortment, 1, 0)

# Create a dummy variable for whether competition is present on a given day
train$CompetitionPresent = ifelse(as.Date(train$Date) < train$CompetitionSinceDate, 0, 1)
train$CompetitionPresent = factor(train$CompetitionPresent, levels = c("0", "1", "missing"))
train[is.na(train$CompetitionPresent), "CompetitionPresent"] = "missing"

# Create a new binary variable indicating if both types of holidays take place on a given day
train$StateHoliday = as.character(train$StateHoliday)
train$BothHolidays = ifelse(train$StateHoliday == train$SchoolHoliday, 1, 0)

# Replace NAs in newly created CompetitionDistance, CompetitionSinceDate, Promo2SinceDate
vars = c("CompetitionDistance", "CompetitionSinceDate", "Promo2SinceDate")

for (i in 1:length(vars)) {
    train[is.na(train[, vars[i]]), vars[i]] = mean(train[, vars[i]], na.rm = T)
}

# Fixing the datatypes of the logical variables
factor.vars = c("StoreType", "Assortment", "Promo2", "DayOfWeek", "Open", "Promo", "StateHoliday", "SchoolHoliday", 
    "PromoInterval", "StoreAssortmentMatch", "BothHolidays")
train[, factor.vars] = lapply(train[, factor.vars], factor)

# Removing unused variables
unused.vars = c("Promo2SinceWeek", "Promo2SinceYear", "CompetitionOpenSinceYear", "CompetitionOpenSinceMonth")
train[, unused.vars] = NULL

# Save a full dataset for data exploration use
saveRDS(train, "FullSet.RDS")
