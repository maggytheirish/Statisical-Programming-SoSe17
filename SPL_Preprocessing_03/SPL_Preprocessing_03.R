##################################################################### 

                     ### Data partitioning ###

##################################################################### 

# Creating separate test set where sales need to be predicted
testdate = as.Date(train$Date) >= "2015-07-20"  # Selecting the last 10 days for forecasting
test = train[testdate == T, ]
train = train[testdate == F, ]  # remove test observations

# Creating train dataset
set.seed(123)
idx = createDataPartition(train$Date, p = 0.05, list = F)
train_set = train[idx, ]

# Fix data type of the date
train_set$Date = as.Date(train_set$Date)
test$Date = as.Date(test$Date)

# creating file to store predictions

Predictions_test = setNames(as.data.frame(test$Sales), "actual")
Predictions_test$benchmark = mean(test$Sales)
saveRDS(Predictions_test, "Predictions_test.RDS")

# Remove the variable Sales that needs to be predicted from the testing dataset
test$Sales = NULL
# Remove the variable customers related to Sales from both traiing and testing datasets
test$Customers = NULL
train_set$Customers = NULL

# Saving datasets
saveRDS(train_set, "train")
saveRDS(test, "test")
