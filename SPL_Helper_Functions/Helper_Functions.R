# Creating a function to load the packages
LoadPackages = function(p) {
    
    for (i in seq_along(p)) {
        if (!require(p[i], character.only = TRUE)) {
            install.packages(p[i])
        }
        library(p[i], character.only = TRUE)
    }
    
}

list.of.packages = c("rpart", "lubridate", "outliers", "rpart.plot", "xgboost", "caret", "caretEnsemble", "randomForest", 
    "e1071", "pROC", "tidyr", "klaR", "car", "devtools", "yamldebugger", "mlbench", "Hmisc", "ggvis", "relaimpo", 
    "formatR", "data.table", "zoo", "ggplot2", "forecast", "reshape2", "pdp", "neuralnet")
sapply(list.of.packages, LoadPackages)
