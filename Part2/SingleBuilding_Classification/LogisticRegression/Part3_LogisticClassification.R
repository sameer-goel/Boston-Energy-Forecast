returnLogistic <- function(csv){
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
    #return(results)
    return()
  }
  
  
  ## Converting base hour class from high/Low to 1 and 0
  Base_Class <- as.numeric(merge.sel$Base_Hour_Class)-1
  data = cbind(Base_Class,scaled)
  
  ## Split validation 75%
  index <- sample(1:nrow(data),round(0.75*nrow(data)))
  train_ <- data[index,]
  test_ <- data[-index,]
  
  
  
  ## Preparing Logistic Regression for classification
  set.seed(2013)
  n <- names(train_)
  f <- as.formula(paste("Base_Class ~", paste(n[!n %in% c("Base_Class","Sea_Level_PressureIn","Humidity","VisibilityMPH")], collapse = " + ")))
  start.time <- Sys.time()
  logit <- glm(f,family = "binomial",data = train_)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  ## Predicting the model output
  test_ <- subset(test_,select = -c(Humidity,Sea_Level_PressureIn,VisibilityMPH))
  
  predValues <- predict(logit, newdata = test_, type = "response")
  
  #Roundig the values
  predValues <- sapply(predValues,round,digits=0)
  
  ## COnfusion matrix
  library(caret)
  library(e1071)
  
  accuracy <- confusionMatrix(predValues, test_$Base_Class)
  results[1,"Accuracy"] <- paste(accuracy$overall[1]*100,"%",sep="")
  
  tocsv <- data.frame(cbind(Model =results[1,"ModelName"],Type = results[1,"Type"] ,t(accuracy$overall),t(accuracy$byClass)))
  
  ## Plotting and saving ROC curve
  library(ROCR)
  pred <- ROCR::prediction(predValues, test_$Base_Class)
  perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr") 
  
  plotName <- paste("LogisticRegresion_Roc/", results[1],".jpeg")
  jpeg(file = plotName)
  ROCR::plot(perf, col=rainbow(10))
  dev.off()
  return(tocsv)
  
}


