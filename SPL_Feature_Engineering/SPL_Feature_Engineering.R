# This script creates new features and splits the dataset for training and testing
train = readRDS("train")  # Load the clean dataset

# Create an extra date varible for future application
train$NewDate = train$Date

# Creating a starting date for Promo2
train$Promo2SinceDate = as.Date(with(train, paste(Promo2SinceYear,Promo2SinceWeek,1, sep="-")),"%Y-%U-%u")
train$Promo2SinceWeek = NULL  # deleting redundant columns
train$Promo2SinceYear = NULL

# Creating a starting date for Competition (assuming it's from the first day of the month)
train$CompetitionSinceDate = as.Date(paste(train$CompetitionOpenSinceYear, train$CompetitionOpenSinceMonth, 1, sep="-"),format="%Y-%m-%d")
train$CompetitionOpenSinceYear = NULL  # deleting redundant columns
train$CompetitionOpenSinceMonth = NULL

# Replacing NAs in CompetitionSinceDate
CompetitionDateMissing = train[is.na(train$CompetitionSinceDate), ]
CompetitionDateMissing = CompetitionDateMissing[!is.na(CompetitionDateMissing$CompetitionDistance), ]
CompetitionDateMissing$CompetitionSinceDate = CompetitionDateMissing$Date  # replacing NAs with the current date
train[is.na(train$CompetitionSinceDate) & !is.na(train$CompetitionDistance),"CompetitionSinceDate"]
     =CompetitionDateMissing$CompetitionSinceDate
train$CompetitionSinceDate = as.Date(train$CompetitionSinceDate)

# Create a new binary variable StoreAssortmentMatch
train$Assortment = as.character(train$Assortment)  # StoreType and Assortment have different levels
train$StoreAssortmentMatch = ifelse(train$StoreType==train$Assortment,1,0)
train$Assortment = as.factor(train$Assortment)
train$StoreAssortmentMatch = as.factor(train$StoreAssortmentMatch)

# Create a dummy variable for whether competition is present on a given day
train$Date = as.Date(train$Date)
train$CompetitionPresent = ifelse(train$Date<train$CompetitionSinceDate,0,1)
train$CompetitionPresent = as.factor(train$CompetitionPresent)
train$CompetitionPresent = factor(train$CompetitionPresent, levels=c(levels(train$CompetitionPresent), "missing"))
form_na = is.na(train$CompetitionPresent)
train[form_na,"CompetitionPresent"] = "missing"

# Create a new binary variable indicating if both types of holidays take place on a given day
train$StateHoliday = as.character(train$StateHoliday)
train$BothHolidays = ifelse(train$StateHoliday==train$SchoolHoliday,1,0)
train$StateHoliday = as.factor(train$StateHoliday)
train$BothHolidays = as.factor(train$BothHolidays)

# Replace NAs in newly created CompetitionDistance, CompetitionSinceDate, Promo2SinceDate
train$CompetitionDistance[is.na(train$CompetitionDistance)] = mean(train$CompetitionDistance, na.rm = TRUE)
train$CompetitionSinceDate[is.na(train$CompetitionSinceDate)] = mean(train$CompetitionSinceDate, na.rm = TRUE)
train$Promo2SinceDate[is.na(train$Promo2SinceDate)] = mean(train$Promo2SinceDate, na.rm = TRUE)
summary(train)   

# Creating separate test set where sales need to be predicted
train$NewDate = as.Date(paste(train$NewDate,sep="-"),format= "%Y-%m-%d")
testdate = train$NewDate>="2015-07-20"  # Selecting the last 10 days for forecasting
newdata.set = train[testdate==T, ]
train = train[testdate==F, ]  # remove test observations
newdata.set$NewDate = NULL

# Adding features with mean sales 

train$Store = as.factor(train$Store)
train$Date = as.Date(paste(train$Date,sep="-"),format= "%Y-%m-%d")
train = separate(train, Date, into = c("Year", "Month","Day"), sep="-")  #splitting the date into separate colums

features.avg = setNames(aggregate(train$Sales, list(train$Store), mean),c("Store","AvgSalesPerStore"))
features.avg$AvgVisitsPerStore = aggregate(train$Customers, list(train$Store), mean)[,2]

features.dow = setNames(aggregate(train$Sales, list(train$Store,train$DayOfWeek), mean),c("Store","DayOfWeek","AvgSalesPerStorePerDayOfWeek"))
features.dow$AvgVisitsPerStorePerDayOfWeek = aggregate(train$Customers, list(train$Store,train$DayOfWeek), mean)[,3]

features.year = setNames(aggregate(train$Sales, list(train$Store,train$Year), mean),c("Store","Year","AvgSalesPerStorePerYear"))
features.year$AvgVisitsPerStorePerYear = aggregate(train$Customers, list(train$Store,train$Year), mean)[,3]

features.mon = setNames(aggregate(train$Sales, list(train$Store,train$Year,train$Month), mean),c("Store","Year","Month","AvgSalesPerStorePerMonth"))
features.mon$AvgVisitsPerStorePerMonth = aggregate(train$Customers, list(train$Store,train$Year,train$Month), mean)[,4]

#Merging new features with the dataset
train = merge(train, features.avg, by="Store")
train = merge(train, features.dow, by=c("Store","DayOfWeek"))
train = merge(train, features.year, by=c("Store","Year"))
train = merge(train, features.mon, by=c("Store","Year","Month"))

# Creating train dataset
train$NewDate = as.factor(train$NewDate)
set.seed(123)
idx = createDataPartition(train$NewDate, p=0.05, list = F)
train.set = train[idx, ]
train.set$NewDate = NULL

# Checking the datasets
summary(train.set)
summary(newdata.set)

# Remove the variable Sales that needs to be predicted from the testing dataset
newdata.set$Sales = NULL

# Remove the variable customers related to Sales from both traiing and testing datasets
newdata.set$Customers = NULL 
train.set$Customers = NULL

# Create testing and training subsets within the train set
idx.test = createDataPartition(train.set$Sales,p=0.8,list=F)
train.final = train.set[idx.test, ]
test.final = train.set[-idx.test, ]

# Pre treatment - use this for modelling

train.final[,c(7,21:28)] = scale(train.final[,c(7,21:28)])
test.final[,c(7,21:28)] = scale(test.final[,c(7,21:28)])
newdata.set[,sapply(newdata_set.v2,is.numeric)] = scale(newdata.set[,sapply(newdata.set,is.numeric)])



# Saving datasets
saveRDS(train.final,"train")
saveRDS(test.final,"test")
saveRDS(newdata.set,"class")
