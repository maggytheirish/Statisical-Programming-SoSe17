#################################################################### 

                    ### Ensemble ###

####################################################################
# The following code trains an xgboost model on the saved predictions from the 
# previous models to pick out the best possible predictions 

# Set the working directory 
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("caret")) install.packages("caret"); library("caret")

# Evaluation metric
rmse = function(actual, pred) {
  error = sqrt(mean((actual - pred)^2))
  return(error)
}

#Modified function to evaluate model
results = function(model,modelname,data, actual) {

      # Predictions
      pred = predict(model, data)
      pred = ifelse(pred <= 0, 0, pred)  ## Removing negative values
      
      cat("\n", "The descriptive statistics of the predicted values", "\n")
      print(summary(pred))
      cat("\n", "The descriptive statistics of the actual values", "\n")
      print(summary(actual))
      
      error = rmse(actual, pred)
      cat(paste0("\n", "The prediction error is : ", error))

      # Error decomposition
      mse = mean((actual - pred)^2)
      bias = (mean(pred) - mean(actual))
      var = mse - (bias^2) 
      percent.var = var/mse
      actual.skew = skewness(actual)  # to check the distributions
      predicted.skew = skewness(pred)
      
      # Saving results
      
      error.matrix = cbind(error, mse, bias, var, percent.var ,actual.skew, predicted.skew)
      cat("\n", "Decomposing the error : ", "\n")
      print(error.matrix)
      
      # Visualization - Plotting the normal curve
      
      x1 = actual
      y1 = dnorm(actual, mean = mean(actual), sd = sd(actual))
      
      x2 = pred
      y2 = dnorm(pred, mean = mean(pred), sd = sd(pred))
      
      cat("\n", "Plotting the distributions of the actual and predicted values - green actual,red predicted")
      
      dist.plot = plot(x1, y1, col = "green", xlim = range(c(x1, x2)), ylim = range(c(y1, 
                                                                                      y2)), xlab = "Sales", ylab = "Density")
      points(x2, y2, col = "red")
      title(main = as.character(modelname))
      legend("topright", legend = c("Actual", "Predicted"), fill = c("green", "red"))
      
      dist.plot
      
      pred = as.vector(round(pred))
      return(pred)}

#Read dataset containing predictions
predictions = readRDS("Predictions_test.RDS")

## Split into two DFs, 50% each, based on Sales
idx_ensemble = createDataPartition(y = predictions$actual, p = 0.5, list = FALSE)

trainset_ensemble = predictions[idx_ensemble,] #using one half of the predictions for training
testset_ensemble = predictions[-idx_ensemble,] #independent testset

#Remove actual values
sales_testset = testset_ensemble$actual # saving actual values for comparison
testset_ensemble$actual = NULL #Removing actual values from the test set

# Train Ensemble

# XGB Model Setup
model.control= trainControl(method = "cv",number = 5,allowParallel = TRUE)
# Setting parameters
xgb.parms.default = expand.grid(nrounds = 400, 
                                 max_depth = 10, 
                                 eta = 0.05, 
                                 gamma = 0,
                                 colsample_bytree = 1,
                                 min_child_weight = 1, 
                                 subsample = 0.6)

# Train Model
xgb.ensemble = caret::train(actual~., data = trainset_ensemble,  
                             method = "xgbTree",
                             tuneGrid = xgb.parms.default,
                             metric = "RMSE", 
                             trControl = model.control)

# Predict 
xgb.ensemble.pred = results(xgb.ensemble,"xgb.ensemble",testset_ensemble,sales_testset)

# Comparing rmse values for the different models on the testset
for(i in c(1:5)){
  error = matrix(NA,nrow=1,ncol = 5)
  colnames(error) = colnames(testset_ensemble)
  for(i in c(1:5)){
  error[1,i] = rmse(sales_testset,as.vector(testset_ensemble[,i]))}
  error = as.data.frame(error)
}

error$ensemble = rmse(sales_testset,xgb.ensemble.pred) #adding rmse from ensemble 
print(error)
