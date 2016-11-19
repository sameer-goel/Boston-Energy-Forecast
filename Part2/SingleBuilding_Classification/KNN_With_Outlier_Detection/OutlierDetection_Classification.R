outlierDetection<- function(csv){
#csv = read.csv('Building 1.5198.1.csv')
  #table(csv$Consumption)
#library(dplyr)
  #csv=name
  mergeData <- csv %>% 
    select(-c(consumption.Kwh.sqm,
              area_floor._m.sqr,vac,BuildingID,
              meternumb,type,Base_hour_Flag,
              City,Conditions,Wind_Direction))
  
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
  #merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  #str(mergeData)
  ## Split validation
  merge.sel <- mergeData
  
  train.size <- 0.8
  train.index <- sample.int(length(merge.sel$Consumption),round(length(merge.sel$Consumption)*train.size))
  train.sample <- merge.sel[train.index,]
  train.val <- merge.sel[-train.index,]  
  
  if(length(unique(csv$Base_Hour_Class)) == 1){
    print("Cannot classify on single categorical variables")
    merge.sel$predicted_base_hour_flag <- "NA"
    merge.sel$score <- "NA"
    merge.sel$Outlier_day <- "NA"
    return(merge.sel)
  }
  
  ## KNN Regression model
  #require(caret)
  set.seed(2016)
  
  ctrl <- trainControl(method="repeatedcv",repeats = 3)
  #hour + Consumption + month + Day.of.Week + weekday + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees
  knn.class <- train(Base_Hour_Class ~ hour + Consumption + month + Day.of.Week + weekday + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees,
                     data= train.sample, 
                     method = "knn", 
                     trControl = ctrl, 
                     preProcess = c("center","scale"), 
                     tuneLength = 1)
  
  merge.sel_OL <- merge.sel
  merge.sel_OL$predicted_base_hour_flag <- predict(knn.class,newdata = merge.sel)
  
  
  merge.sel_OL <- merge.sel_OL %>% 
                  mutate(score = ifelse(merge.sel_OL$Base_Hour_Class == merge.sel_OL$predicted_base_hour_flag, 0 ,1)) 
  ### Detectig outlier day
  new_data <- merge.sel_OL %>% 
                  group_by(date) %>% 
                  mutate(Outlier_day = ifelse(sum(score) > 5, "TRUE", "FALSE"))
  
  return(as.data.frame(new_data))

}