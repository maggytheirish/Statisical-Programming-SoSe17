##################################################################### 

                     ### Data partitioning ###

##################################################################### 
#Loading packages 
if (!require("caret")) install.packages("caret"); library("caret")

# Loading the dataset
FullSet = readRDS("FullSet.RDS")

# Creating separate test set where sales need to be predicted
testdate = as.Date(FullSet$Date) >= "2015-07-20"  # Selecting the last 10 days for forecasting
test = FullSet[testdate == T, ]
FullSet = FullSet[testdate == F, ]  # remove test observations

# Creating train dataset using 5 percent of the dataset for training
set.seed(123)
idx = createDataPartition(FullSet$Date, p = 0.05, list = F)
train = FullSet[idx, ]

# Fix data type of the date
train$Date = as.Date(train$Date)
test$Date = as.Date(test$Date)

# Creating file to store predictions
Predictions_test = setNames(as.data.frame(test$Sales), "actual")
Predictions_test$benchmark = mean(test$Sales)
save(Predictions_test, "Predictions_test.RDS")

# Remove the variable Sales that needs to be predicted from the testing dataset
test$Sales = NULL
# Remove the variable Customers correlated with Sales from both training and testing datasets
test$Customers = NULL
train$Customers = NULL

# Saving datasets
saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")
