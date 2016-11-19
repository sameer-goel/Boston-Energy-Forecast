returnNeuralNetwork <- function(csv){
  library(forecast)
  
  #write.csv(`Building 1.5198.1`,"Building 1.5198.1.csv")
  #csv <- name
  
  
  require(dplyr)
  mergeData <- csv %>% select(-c(date,consumption.Kwh.sqm,area_floor._m.sqr,vac,BuildingID,meternumb,type,Base_hour_Flag,City,Base_Hour_Class))
  
  ## Converting non numeric features to numeric
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  
  
  #str(mergeData)
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  
  ## Checking to see if there are any missing data
  #apply(merge.sel,2,function(x) sum(is.na(x)))
  
 
  
  ## Split validation 75%
  index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))
  
  ## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
  maxs <- apply(merge.sel, 2, max) 
  mins <- apply(merge.sel, 2, min)
  
  scaled <- as.data.frame(scale(merge.sel, center = mins, scale = maxs - mins))
  
  train_ <- scaled[index,]
  test_ <- scaled[-index,]
  
  
  ##### Baseline model
  # In the absence of any predictor, all we have is the dependent variable (Consumption).
  # What would be our best guess if we had to predict the amount of Consumption, on a given day, in the test set? I would say that the mean of the Consumption values, in the training data, is the best value we can come up with.
  # Recall we can only use the training data to build models; the testing data is only there to evaluate them, after making the predictions.
  
  best.guess <- mean(train_$Consumption) * (max(merge.sel$Consumption)-min(merge.sel$Consumption))+min(merge.sel$Consumption)
  RMSE.baseline <- sqrt(mean((best.guess-(test_$Consumption*(max(merge.sel$Consumption)-min(merge.sel$Consumption))+min(merge.sel$Consumption)))^2))
  MAE.baseline <- mean(abs(best.guess-(test_$Consumption*(max(merge.sel$Consumption)-min(merge.sel$Consumption))+min(merge.sel$Consumption))))
  
  ### Putting the results into the data frame
  results = as.data.frame(NULL)
  results[1,"ModelName"] <- unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = "."))
  results[1,"Type"] <- unique(csv$type)
  results[1,"RMSE.baseline"] <- RMSE.baseline
  results[1,"MAE.baseline"] <- MAE.baseline
  
  ## Preparing Neural Network 
  #set.seed(2013)
  library(neuralnet)
  n <- names(train_)
  f <- as.formula(paste("Consumption ~", paste(n[!n %in% c("Consumption","Sea_Level_PressureIn","Humidity","VisibilityMPH")], collapse = " + ")))
  start.time <- Sys.time()
  nn <- neuralnet(f,data=train_,hidden=1,linear.output=T,stepmax = 1e+06)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  ## Predicting the model. Output need to be normalized, need to scale back in order to make a meaningful comparison
  test_ <- subset(test_,select = -c(Humidity,Sea_Level_PressureIn,VisibilityMPH))
  
  #Computing the value would require to pass only the features used in the formula
  pr.nn <- neuralnet::compute(nn,test_[,c("hour","month","Day.of.Week","weekday","Holiday","base_hr_usage","TemperatureF","Dew_PointF","Wind_SpeedMPH","WindDirDegrees")])
   
  #scaling down the predicted results
  pr.nn_ <- pr.nn$net.result*(max(merge.sel$Consumption)-min(merge.sel$Consumption))+min(merge.sel$Consumption)
  
  #Scaling down the actual consumptions
  test.r <- (test_$Consumption)*(max(merge.sel$Consumption)-min(merge.sel$Consumption))+min(merge.sel$Consumption)
  
  
  ## Evaluating the model's accuracy
  resultset <- data.frame(actual = test.r, prediction = pr.nn_)
  accuracy <- accuracy(resultset$prediction, resultset$actual)
  
  ## Plotting the result
  #plot(test.r,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
  #abline(0,1,lwd=2)
  #legend('bottomright',legend='NN',pch=18,col='red', bty='n')
  
  ## Adding coefficients to the results tab
  results[1,"RMSE.model"] <- accuracy[2]
  results[1,"MAE.model"] <- accuracy[3]
  results[1,"ME.model"] <- accuracy[1]
  results[1,"MAPE.model"] <- accuracy[4]
  
  
  return(results)
}