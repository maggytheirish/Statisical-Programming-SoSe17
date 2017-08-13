# Creating a function to load the packages

load.packages = function(p) {
    
    for (i in seq_along(p)) {
        if (!require(p[i], character.only = TRUE)) {
            install.packages(p[i])
        }
        library(p[i], character.only = TRUE)
    }
    
}

### Loading the datasets ###

load.multiple.files <- function(path, pattern) {
    
    all.csv.files <- list()
    list.filenames <- list.files(path, pattern = glob2rx(pattern))
    
    for (i in 1:length(list.filenames)) {
        all.csv.files[[i]] <- read.csv(paste(path, list.filenames[i], sep = "/"), sep = ",")
    }
    
    names(all.csv.files) <- list.filenames
    
    return(all.csv.files)
}

# Function to save the predictions

save.prediction <- function(modelname, modelresults) {
    
    # Loading the datasets
    
    Predictions_test <- readRDS("Predictions_test.RDS")
    
    # check if number of observations is equal and save results
    
    if (nrow(Predictions_test) != length(modelresults)) {
        print("mismatch in number of rows")
    } else (Predictions_test[, modelname] <- modelresults)
    
    saveRDS(Predictions_test, "Predictions_test.RDS")
}

# Evaluation metric

rmse <- function(actual, pred) {
    error <- sqrt(mean((actual - pred)^2))
    return(error)
}


