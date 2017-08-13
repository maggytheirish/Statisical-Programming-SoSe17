#################################################################### 

          ### Modelling Framework for Tuning models ###

#################################################################### 
# Set the working directory 
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
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

# This function tunes the base models (rf,xgb,lr,nnet) over a specified tune grid using the
# caret package Function call example : model.training(data=training dataset,test=testing
# dataset, method = c('rf','nnet','xgb','lr'))

model.training = function(data, test, method) {
    
    # Load required packages
    if (!require("caret")) install.packages("caret"); library(caret)
    if (!require("doParallel")) install.packages("doParallel"); library(doParallel)
    
    # Check method specification
    methods = c("xgbTree", "nnet", "lm", "rf")
    
    if (!is.character(method)) {
        stop("method must be a string")
    }
    if (!is.element(method, methods)) {
        stop("specified method is not 'xgboost', 'nnet', 'lm' or 'rf'")
    }
    
    # Parallelization
    cl = makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    
    ## General controls for model tuning for all models 
    model.control= trainControl( method = "cv", # cross validation 
                                 number = 5, # number of folds in cross validation 
                                 allowParallel = TRUE, # Enable parallelization if available 
                                 returnData = TRUE # We will use this to plot partial dependence 
                                )
    
    # Tune grids for hyperparameters for each model
    if (method == "xgbTree") {
        tuneGrid = expand.grid(nrounds = c(100, 200, 400), max_depth = c(6, 8, 10), eta = c(0.01, 
            0.05), gamma = 0, colsample_bytree = c(0.3, 0.5, 1), min_child_weight = 1, subsample = c(0.5, 
            0.6, 0.8, 1))
    } else if (method == "rf") {
        n = round(ncol(data)/3, 0)
        nd = n - 5
        nup = n + 5
        tuneGrid = expand.grid(mtry = nd:nup, ntree = 500)
    } else if (method == "nnet") {
        tuneGrid = expand.grid(.decay = c(0, 10^seq(-3, 0, 1)), .size = c(3, 4, 5))
    } else if (method == "lm") {
        tuneGrid = NULL
    }
    
    # start time
    time.start = Sys.time()
    print(paste0("Model started at: ", time.start))
    
    
    # train model
    default.model = caret::train(Sales ~ ., data = data, method = method, tuneGrid = tuneGrid, 
        metric = "RMSE", trControl = model.control)
    
    # time spent
    time.end = Sys.time()
    dur = time.end - time.start
    print(dur)
    
    stopCluster(cl)
    return(default.model)
}

# Tuning the models
lm.model = model.training(train, test, "lm")
nnet.model = model.training(train, test, "nnet")
rf.model = model.training(train, test, "rf")
xgboost.model = model.training(train, test, "xgboost")

# Predict on the test set
lm.res = predict(lm.model,test)
nnet.res = predict(nnet.model,test)
rf.res = predict(rf.model,test)
xgboost.res = predict(xgboost.model,test)
