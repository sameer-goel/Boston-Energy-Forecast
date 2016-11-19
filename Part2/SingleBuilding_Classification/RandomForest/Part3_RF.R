returnRandomForest <- function(csv){
csv=name
mergeData <- csv %>% 
  select(-c(date,consumption.Kwh.sqm,
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

### Putting the results into the data frame
results = as.data.frame(NULL)
results[1,"ModelName"] <- unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = "."))
results[1,"Type"] <- unique(csv$type)

if(length(unique(csv$Base_Hour_Class)) == 1){
  print("Cannot classify on single categorical variables")
  results[1,"Accuracy"] <- "NA"
  return(results)
}
## RF classification model
set.seed(2017)

ctrl <- trainControl(method="repeatedcv",repeats = 3)

rf.class <- train(Base_Hour_Class ~ hour + Consumption + month + Day.of.Week + weekday + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees,
                   data= train.sample, 
                   method = "rf", 
                   trControl = ctrl, 
                   preProcess = c("center","scale"), 
                   tuneLength = 1)

rfPredict <- predict(rf.class,newdata = train.val )
accuracy <- confusionMatrix(rfPredict, train.val$Base_Hour_Class)
results[1,"Accuracy"] <- accuracy$overall[1]*100

rfPredict <- predict(knn.class,newdata = train.val , type="prob")
rf_roc <-  roc(train.val$Base_Hour_Class, rfPredict[,1], levels = rev(train.val$Base_Hour_Class))

plotName <- paste("RF_Roc/", results[1],".jpeg")
jpeg(file = plotName)
myplot <- plot(knn_roc, type="S", print.thres= 0.5)
dev.off()
return(results)
}