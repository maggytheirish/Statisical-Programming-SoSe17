#################################################################### 

                        ### Model Evaluation ###

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

# This is a self designed custom wrapper that evaluates the predictive accuracy of both
# classification and regression model. This is an 'interactive' function which takes inprocess
# inputs from the user.  Function call example - evaluate(model= name of model,data = testing
# dataset, actual = actual values for comparision) evaluate(lr,test,Predictions_test$actual)

evaluate = function(model, data, actual) {
    
    type = readline(prompt = "Choose - Classification or Regression")
    
    if (type == "Classification") {
        
        classifier = readline(prompt = "Type in the name of the classifier : (eg. lr, xgb)")
        
        if (classifier == "lr") {
            probabilities = predict(model, newdata = data)
        } else {
            probabilities = predict(model, newdata = data)
        }
        
        stats = summary(probabilities)
        print("Descriptive statistics for the prediction probabilities : ")
        print(stats)
        
        threshold = readline(prompt = "Do you want to use a specific threshold for classification?
                      Type in the numerical threshold value if not type 'no'")
        
        if (threshold == "no") {
            print("You have chosen to use a default threshold")
            print("Classification using 0.5,mean of the probabilities as thresholds")
            
            pred_random = ifelse(probabilities >= 0.5, pos, neg)
            pred_mean = ifelse(probabilities >= stats[3], pos, neg)
            
            # Display results
            print("Random Classification")
            print(table(pred_random))
            
            print("Threshold - Mean")
            print(table(pred_mean))
            
        } else {
            pred_user = ifelse(probabilities >= as.numeric(threshold), pos, neg)
            
            # Display results
            cat(paste0("\n", "Classification with user defined threshold", as.numeric(threshold)))
            print(table(pred_user))
            
        }
        
    } else {
        if (type == "Regression") {
            
            # Predictions
            pred = predict(model, data)
            pred = ifelse(pred <= 0, 0, pred)  ## Removing negative values
            
            cat("\n", "The descriptive statistics of the predicted values", "\n")
            print(summary(pred))
            cat("\n", "The descriptive statistics of the actual values", "\n")
            print(summary(actual))
            
            # Metric
            metric = readline(prompt = "Which metric would you like to use for error analysis?
                       Choose btw rmse,user. If you want to use a specific metric please
                       add it to the helper function and name it as user.")
            
            if (metric == "rmse") {
                
                error = rmse(actual, pred)
                cat(paste0("\n", "The prediction error is : ", error))
            } else {
                error = user(actual, pred)
            }
            
            
            # Error decomposition
            mse = mean((actual - pred)^2)
            bias = abs(mean(actual) - mean(pred))
            variance = var(actual, pred)  #mse = bias^2 + var
            unexplained = abs(mse - (bias^2 + variance))/mse  #what remains after bias and variance
            actual.skew = skewness(actual)  # to check the distributions
            predicted.skew = skewness(pred)
            
            # Saving results
            
            error.matrix = cbind(error, mse, bias, variance, unexplained, actual.skew, predicted.skew)
            cat("\n", "Decomposing the error : ", "\n")
            print(error.matrix)
            
            # Visualization - Plotting the normal curve
            
            x1 = actual
            y1 = dnorm(actual, mean = mean(actual), sd = sd(actual))
            
            x2 = pred
            y2 = dnorm(pred, mean = mean(pred), sd = sd(pred))
            
            cat("\n", "Plotting the distributions of the actual and predicted values - green actual,red predicted")
            
            dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)), ylim = range(c(y1, 
                y2)), xlab = "Sales", ylab = "Normalized Sales")
            points(x2, y2, col = "red")
            legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
            
            dist.plot
            
            pred = as.vector(round(pred))
            return(pred)
            
        }
    }
    
}

# Run the models with optimal parameters
lr.model = lm(Sales~.,train)
lr.res = evaluate(lr.model, test, Prediction_test$actual)
save.predictions("lr.optimal", lr.res)

nnet.model = nnet(Sales~.,train, decay = 1, size = 3)
nnet.res = evaluate(nnet.model, test, Prediction_test$actual)
save.predictions("nnet.optimal", nnet.res)

rf.model = rf(Sales~.,train, ntree = 500, mtry = 8)
rf.res = evaluate(rf.model, test, Prediction_test$actual)
save.predictions("rf.optimal", rf.res)

xgboost.model = xgboost(Sales~.,train, nrounds = 400, max_depth = 10, eta = 0.05, gamma = 0, 
                        colsample_bytree = 1, min_child_weight = 1, subsample = 0.6)
xgboost.res = evaluate(xgboost.model, test, Prediction_test$actual)
save.predictions("xgboost.optimal", xgboost.res)
