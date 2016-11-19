returnRandomForestOutlier <- function(csv,ntree = 100){
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
  
  
  #str(mergeData)
  
  #require(psych)
  #pairs.panels(merge.sel,col="red")
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  
  ## Split validation
  train.size <- 0.8
  train.index <- sample.int(length(merge.sel$Consumption),round(length(merge.sel$Consumption)*train.size))
  train.sample <- merge.sel[train.index,]
  train.val <- merge.sel[-train.index,]
  
  set.seed(2014)
  
  
  ## Random forest model
  rf <- randomForest(Consumption ~ hour + month + Day.of.Week + weekday + Holiday + base_hr_usage 
                     + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees,
                     data = train.sample, 
                     importance = TRUE,
                     ntree=ntree)
 
  ## Predicting the model
  test.pred.forest <- predict(rf,train.val)
  accuracy <- accuracy(test.pred.forest, train.val$Consumption)
  
  
  ## Computing the predictions and residuals. Saving the results 
  output <- data.frame(Actual_Kwh = train.val$Consumption,Predicted_Kwh = test.pred.forest,Residuals = abs(train.val$Consumption-test.pred.forest))
  
  ## Calculating the standard deviation on the residuals
  sd <- sd(test.pred.forest, na.rm = FALSE)
  
  ## Calculating outlier
  output <- output %>% mutate(Outliers = ifelse(output$Residuals > 2*sd,"Outlier",""))
  name <- paste("RF_Residuals/",unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = ".")),".csv")
  write.csv(output,name)
  return()
 
}