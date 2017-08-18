#################################################################### 

          ### Modelling Framework for Tuning models ###

#################################################################### 
# This function tunes the base models (rf,xgb,lr,nnet) over a specified tune grid using the
# caret package Function call example : model.training(data=training dataset,test=testing
# dataset, method = c("xgbTree","avNNet", "lm", "rf"))

# Set the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Function to save the predictions
save.prediction = function(modelname, modelresults) {
    
    # Loading the datasets
    Predictions_test = readRDS("Predictions_test.RDS")
    
    # check if number of observations is equal and save results
    if (nrow(Predictions_test) != length(modelresults)) {
        stop("mismatch in number of rows")
    } else (Predictions_test[, modelname] = modelresults)
    
    saveRDS(Predictions_test, "Predictions_test.RDS")
}

# Evaluation metric
rmse = function(actual, pred) {
    error = sqrt(mean((actual - pred)^2))
    return(error)
}

# Funtion for model tuning
model.training = function(data, test, method) {
    
    # Load required packages
    if (!require("caret")) 
        install.packages("caret")
    library(caret)
    if (!require("doParallel")) 
        install.packages("doParallel")
    library(doParallel)
    
    # Check method specification
    methods = c("xgbTree", "avNNet", "lm", "rf")
    
    if (!is.character(method)) {
        stop("method must be a string")
    }
    if (!is.element(method, methods)) {
        stop("specified method is not 'xgboost', 'nnet', 'lm' or 'rf'")
    }
    
    # Parallelization
    cl = makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    
    # General controls for model tuning for all models
    model.control = trainControl(method = "cv", number = 5, allowParallel = TRUE, returnData = TRUE)
    
    # Tune grids for hyperparameters for each model
    if (method == "xgbTree") {
        tuneGrid = expand.grid(nrounds = c(100, 200, 400), max_depth = c(6, 8, 10), eta = c(0.01, 0.05), 
            gamma = 0, colsample_bytree = c(0.3, 0.5, 1), min_child_weight = 1, subsample = c(0.5, 0.6, 0.8, 
                1))
    } else if (method == "rf") {
        tuneGrid = expand.grid(mtry = 4:12)  #default mtry for regression = sqrt(no of cols)
    } else if (method == "avNNet") {
        tuneGrid = expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = c(3, 4, 5), bag = F)
    } else if (method == "lm") {
        tuneGrid = NULL
    }
    
    # start time
    time.start = Sys.time()
    print(paste0("Model started at: ", time.start))
    
    
    # train model
    if (method == "avNNet") {
        default.model = caret::train(Sales ~ ., data = data, method = method, tuneGrid = tuneGrid, maxit = 1000, 
            preProc = c("center", "scale"), linout = 1, MaxNWts = 5000, metric = "RMSE", trControl = model.control)
    } else {
        default.model = caret::train(Sales ~ ., data = data, method = method, tuneGrid = tuneGrid, metric = "RMSE", 
            trControl = model.control)
    }
    
    # time spent
    time.end = Sys.time()
    dur = time.end - time.start
    print(dur)
    
    # predict on independent test set
    default.pred = predict(default.model, newdata = test)
    
    # model performance
    cat("\n", "The best tune is :", "\n")
    print(default.model$bestTune)
    
    Predictions_test = readRDS("Predictions_test.RDS")
    error = rmse(Predictions_test$actual, default.pred)
    
    print(paste0("The RMSE on the test set: ", error))
    
    # saving the model and the predictions
    save.prediction(paste0(method, ".", "tune"), default.pred)
    saveRDS(default.model, paste0(method, "_", "model", ".", "RDS"))
    
    stopCluster(cl)  # to clear up the cores
    registerDoSEQ()
    return(default.model)
}


# Read in the datasets
train = readRDS("train.RDS")
train = train[1:1000, ]  # the model tuning takes a while,using 1000 observations for checking the code
test = readRDS("test.RDS")

# Function call for tuning the models
lm.model = model.training(train, test, "lm")
nnet.model = model.training(train, test, "avNNet")
rf.model = model.training(train, test, "rf")
xgboost.model = model.training(train, test, "xgbTree")
