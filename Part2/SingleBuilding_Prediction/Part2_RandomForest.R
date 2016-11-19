returnRandomForest <- function(csv,ntree = 100){
  library(forecast)
  
  library(randomForest)
  #write.csv(`Building 1.5198.1`,"Building 1.5198.1.csv")
  #csv <- read.csv("Building 1.5198.1.csv")
  
 
  require(dplyr)
  mergeData <- csv %>% select(-c(date,consumption.Kwh.sqm,area_floor._m.sqr,vac,BuildingID,meternumb,type,Base_hour_Flag,City,Base_Hour_Class))
  
  ## Converting non numeric features to numeric
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  
  
  str(mergeData)
  
  #require(psych)
  #pairs.panels(merge.sel,col="red")
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  
  ## Split validation
  train.size <- 0.8
  train.index <- sample.int(length(merge.sel$Consumption),round(length(merge.sel$Consumption)*train.size))
  train.sample <- merge.sel[train.index,]
  train.val <- merge.sel[-train.index,]
  
  ##### Baseline model
  # In the absence of any predictor, all we have is the dependent variable (Consumption).
  # What would be our best guess if we had to predict the amount of Consumption, on a given day, in the test set? I would say that the mean of the Consumption values, in the training data, is the best value we can come up with.
  # Recall we can only use the training data to build models; the testing data is only there to evaluate them, after making the predictions.
  
  best.guess <- mean(train.sample$Consumption) 
  RMSE.baseline <- sqrt(mean((best.guess-train.val$Consumption)^2))
  MAE.baseline <- mean(abs(best.guess-train.val$Consumption))
  
  ### Putting the results into the data frame
  results = as.data.frame(NULL)
  results[1,"ModelName"] <- unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = "."))
  results[1,"Type"] <- unique(csv$type)
  results[1,"RMSE.baseline"] <- RMSE.baseline
  results[1,"MAE.baseline"] <- MAE.baseline
  
  
  set.seed(2014)
  
  
  ## Random forest model
  rf <- randomForest(Consumption ~ hour + month + Day.of.Week + weekday + Holiday + base_hr_usage 
                     + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees,
                     data = train.sample, 
                     importance = TRUE,
                     ntree=ntree)
  
  #imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
  #names(imp) <- "% Inc MSE"
  
  ## Predicting the model
  test.pred.forest <- predict(rf,train.val)
  accuracy <- accuracy(test.pred.forest, train.val$Consumption)
  
  ## Adding coefficients to the results tab
  results[1,"RMSE.model"] <- accuracy[2]
  results[1,"MAE.model"] <- accuracy[3]
  results[1,"ME.model"] <- accuracy[1]
  results[1,"MAPE.model"] <- accuracy[4]
  
  
  return(results)
}