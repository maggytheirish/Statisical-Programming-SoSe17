#################################################################### 

                        ### Model Evaluation ###

#################################################################### 
# Set the working directory 
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!require("e1071")) install.packages("e1071"); library("e1071")

# Evaluation metric
rmse = function(actual, pred) {
    error = sqrt(mean((actual - pred)^2))
    return(error)
}

# This is a self designed custom wrapper that evaluates the predictive accuracy of both
# classification and regression model. This is an 'interactive' function which takes inprocess
# inputs from the user.  Function call example - evaluate(model= name of model,data = testing
# dataset, actual = actual values for comparision) evaluate(lr,test,Predictions_test$actual)

evaluate = function(model,modelname,data, actual) {
    
  print("This is an interactive function. Please type in your responses in the console :")  
  type = readline(prompt = "Type your response - Classification or Regression : ")
    
  #Error handling
  if(type!="Classification"&type!="Regression"){
    stop("Choose an appropriate method. Check for typos!")}
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
            cat("\n", "Which metric would you like to use for error analysis?","\n",
                "If you want to use a specific metric please add it to the helper function and name it as user.")
            metric = readline(prompt =  "Choose metric - rmse,user : ")
            #Error handling
            if(metric!="rmse"&metric!="user"){
              stop("Choose an appropriate method. Check for typos!")}
            
            if (metric == "rmse") {
                
                error = rmse(actual, pred)
                cat(paste0("\n", "The prediction error is : ", error))
            } else {
                error = user(actual, pred)
            }
            
            
            # Error decomposition
            mse = mean((actual - pred)^2)
            bias = (mean(pred) - mean(actual))
            var = mse - (bias^2) #(Decomposition of mse)
            actual.skew = skewness(actual)  # to check the distributions
            predicted.skew = skewness(pred)
            
            # Saving results
            
            error.matrix = cbind(error, mse, bias,var, actual.skew, predicted.skew)
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
    }
    
}

#Read the datsets
train = readRDS("train.RDS")
test = readRDS("test.RDS")
Predictions_test = readRDS("predictions_test.RDS")

#Loading the saved models
lr.model = lm(Sales~.,train)
nn.model = readRDS("avNNet_model.RDS")
xgb.model = readRDS("xgbTree_model.RDS")
#rf.model = randomForest(Sales~.,train,ntree=500,mtry=12) #This might take a while, use smaller dataset

#Evaluate the predictions
lr.res = evaluate(lr.model,"Linear Regression",test,Predictions_test$actual)
nn.res = evaluate(nn.model,"Neural Network",test,Predictions_test$actual)
rf.res = evaluate(rf.model,"Random Forest",test,Predictions_test$actual)
xgb.res = evaluate(xgb.model,"Gradient Boosting",test,Predictions_test$actual)
