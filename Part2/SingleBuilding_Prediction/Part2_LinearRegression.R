returnLinearModel <- function(csv){
  #csv <- read.csv("Building 1.5198.1.csv", stringsAsFactors = FALSE
  
  mergeData <- csv %>% 
               select(-c(date,consumption.Kwh.sqm,
                         area_floor._m.sqr,vac,BuildingID,
                         meternumb,type,Base_hour_Flag,
                         City,Base_Hour_Class))
  
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)

  #str(mergeData)
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))

  ## Split validation
  train.size <- 0.8
  train.index <- sample.int(length(merge.sel$Consumption),round(length(merge.sel$Consumption)*train.size))
  train.sample <- merge.sel[train.index,]
  train.val <- merge.sel[-train.index,]  
  
  best.guess <- mean(train.sample$Consumption) 
  RMSE.baseline <- sqrt(mean((best.guess-train.val$Consumption)^2))
  MAE.baseline <- mean(abs(best.guess-train.val$Consumption))
  
  ### Putting the results into the data frame
  results = as.data.frame(NULL)
  results[1,"ModelName"] <- unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = "."))
  results[1,"Type"] <- unique(csv$type)
  results[1,"RMSE.baseline"] <- RMSE.baseline
  results[1,"MAE.baseline"] <- MAE.baseline
  
  ## Linear Regression model
  set.seed(2014)
  lin.reg <- lm(formula = Consumption ~ hour + month + Day.of.Week + weekday + Holiday 
                + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees,
                data = train.sample)
  
  library(forecast)
  train.val$Pred_Consumption <- predict(lin.reg, newdata = train.val)
  accuracy <- accuracy(train.val$Pred_Consumption,train.val$Consumption)
  
  ## Adding coefficients to the results tab
  results[1,"RMSE.model"] <- accuracy[2]
  results[1,"MAE.model"] <- accuracy[3]
  results[1,"ME.model"] <- accuracy[1]
  results[1,"MAPE.model"] <- accuracy[4]
  
  return(results)
}


