returnNeuralNetworkclass <- function(csv){
  library(forecast)
  
  #write.csv(`Building 1.5198.1`,"Building 1.5198.1.csv")
  #csv <- name
  
  
  require(dplyr)
  mergeData <- csv %>% select(-c(date,consumption.Kwh.sqm,area_floor._m.sqr,vac,BuildingID,meternumb,type,City))
  
  ## Converting non numeric features to numeric
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
  
  #str(mergeData)
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  
  ## Checking to see if there are any missing data
  #apply(merge.sel,2,function(x) sum(is.na(x)))
  
  ## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
  maxs <- apply(merge.sel[,-9], 2, max) 
  mins <- apply(merge.sel[,-9], 2, min)
  scaled <- as.data.frame(scale(merge.sel[,-9], center = mins, scale = maxs - mins))
  
  
  ### Putting the results into the data frame
  results = as.data.frame(NULL)
  results[1,"ModelName"] <- unique(paste(csv$vac,csv$BuildingID,csv$meternumb,sep = "."))
  results[1,"Type"] <- unique(csv$type)
  
  
  ## Handling if the classifier is unique
  if(length(unique(csv$Base_Hour_Class)) == 1){
    print("Cannot classify on single categorical variables")
    results[1,"Accuracy"] <- "NA"
    return(results)
  }
  
  
  ## Converting base hour class from high/Low to 1 and 0
  Base_Class <- as.numeric(merge.sel$Base_Hour_Class)-1
  data = cbind(Base_Class,scaled)
  
  ## Split validation 75%
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train_ <- data[index,]
  test_ <- data[-index,]
  
  
  
  ## Preparing Neural Network for classification
  #set.seed(2012)
  library(neuralnet)
  n <- names(train_)
  f <- as.formula(paste("Base_Class ~", paste(n[!n %in% c("Consumption","Day.of.Week","month","TemperatureF","Base_Class","Sea_Level_PressureIn","weekday","Humidity","VisibilityMPH","Dew_PointF","Holiday","Wind_SpeedMPH","WindDirDegrees")], collapse = " + ")))
  start.time <- Sys.time()
  print(start.time)
  nn <- neuralnet(f,data=train_,linear.output = F,hidden = c(4,2),stepmax = 1e+05, threshold = 0.1)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  ## Predicting the model output
  #test_ <- subset(test_,select = -c(Humidity,Sea_Level_PressureIn,VisibilityMPH))
  
  #Computing the value would require to pass only the features used in the formula
  #pr.nn <- neuralnet::compute(nn,test_[,c("hour","Consumption","month","Day.of.Week","weekday","Base_hour_Flag","Holiday","base_hr_usage","TemperatureF","Dew_PointF","Wind_SpeedMPH","WindDirDegrees")])
  pr.nn <- neuralnet::compute(nn,test_[,c("Base_hour_Flag","hour","base_hr_usage")])
  
  #Roundig the values
  pr.nn$net.result <- sapply(pr.nn$net.result,round,digits=0)
  
  ## COnfusion matrix
  library(caret)
  library(e1071)
  
  accuracy <- confusionMatrix(pr.nn$net.result, test_$Base_Class)
  results[1,"Accuracy"] <- paste(accuracy$overall[1]*100,"%",sep="")
  
  ## Plotting and saving ROC curve
  library(ROCR)
  pred <- ROCR::prediction(pr.nn$net.result, test_$Base_Class)
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr") 
  
  plotName <- paste("NeuralNetwork_Roc/", results[1],".jpeg")
  jpeg(file = plotName)
  ROCR::plot(perf, col=rainbow(10))
  dev.off()
  return(results)
  
}