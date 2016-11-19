############ LINEAR REGRESSION (All Model)  

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
str(csv)
#View(csv[which(is.na(csv$TemperatureF)),])
require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$consumption.Kwh.sqm <- as.numeric(mergeData$consumption.Kwh.sqm)
mergeData$area_floor._m.sqr <- as.numeric(mergeData$area_floor._m.sqr)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)

#str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
merge.sel <- mergeData
#apply(merge.sel,2,function(x) sum(is.na(x)))
## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
#data = cbind(Base_Class,scaled)

train.sample <- scaled[index,]
train.val <- scaled[-index,]  

best.guess <- mean((train.sample$consumption.Kwh.sqm) * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm))
value <- train.val$consumption.Kwh.sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"
results[1,"RMSE.baseline"] <- sqrt(mean((best.guess-value)^2))
results[1,"MAE.baseline"] <- mean(abs(best.guess-value))

## Linear Regression model
set.seed(2011)
lin.reg <- lm(consumption.Kwh.sqm ~ Day.of.Week + weekday + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees + area_floor._m.sqr, data = train.sample)

library(forecast)
train.val$Pred_Consumption_kwh_sqm <- predict(lin.reg, newdata = train.val)
train.val$Pred_Consumption_kwh_sqm <- train.val$Pred_Consumption_kwh_sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)
train.val$consumption.Kwh.sqm <- train.val$consumption.Kwh.sqm *  (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)
accuracy <- accuracy(train.val$Pred_Consumption,train.val$consumption.Kwh.sqm)

## Adding coefficients to the results tab
results[1,"RMSE.model"] <- accuracy[2]
results[1,"MAE.model"] <- accuracy[3]
results[1,"ME.model"] <- accuracy[1]
results[1,"MAPE.model"] <- accuracy[4]
results[1,"Adjusted R Square"] <- summary(lin.reg)$r.squared

write.csv(train.val,"LinearModel_AllBuildings_Output.csv")
write.csv(results,"All Buildings_LinearRegression.csv")

############ RANDOM FOREST (All Model)  

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
str(csv)
#View(csv[which(is.na(csv$TemperatureF)),])
#require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$consumption.Kwh.sqm <- as.numeric(mergeData$consumption.Kwh.sqm)
mergeData$area_floor._m.sqr <- as.numeric(mergeData$area_floor._m.sqr)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)

#str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
merge.sel <- mergeData
#apply(merge.sel,2,function(x) sum(is.na(x)))
## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
#data = cbind(Base_Class,scaled)

train.sample <- scaled[index,]
train.val <- scaled[-index,]  

best.guess <- mean((train.sample$consumption.Kwh.sqm) * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm))
value <- train.val$consumption.Kwh.sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"
results[1,"RMSE.baseline"] <- sqrt(mean((best.guess-value)^2))
results[1,"MAE.baseline"] <- mean(abs(best.guess-value))




### Random forest parallel processing
x <- train.sample[,-c(1,2,5,8,12,13,14)]
y <- train.sample[,8]


library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

library(caret)
fitControl <- trainControl(method = "cv",number = 2,allowParallel = TRUE)
fit <- caret::train(x,y, method="rf",trControl = fitControl)

## RANDOM FOREST model
library(ParallelForest)

set.seed(2012)
require(randomForest)
library(doParallel)
registerDoParallel(cores=detectCores(all.tests=TRUE))
rf <- randomForest(consumption.Kwh.sqm ~ Day.of.Week + weekday + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees + area_floor._m.sqr,
                   data = train.sample, 
                   importance = TRUE,
                   ntree=30)

library(forecast)
train.val$Pred_Consumption_kwh_sqm <- predict(lin.reg, newdata = train.val)
train.val$Pred_Consumption_kwh_sqm <- train.val$Pred_Consumption_kwh_sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)
train.val$consumption.Kwh.sqm <- train.val$consumption.Kwh.sqm *  (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)
accuracy <- accuracy(train.val$Pred_Consumption,train.val$consumption.Kwh.sqm)

## Adding coefficients to the results tab
results[1,"RMSE.model"] <- accuracy[2]
results[1,"MAE.model"] <- accuracy[3]
results[1,"ME.model"] <- accuracy[1]
results[1,"MAPE.model"] <- accuracy[4]

write.csv(results,"All Buildings_LinearRegression.csv")


######### NEURAL NETWORK
csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
str(csv)
#View(csv[which(is.na(csv$TemperatureF)),])
#require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$consumption.Kwh.sqm <- as.numeric(mergeData$consumption.Kwh.sqm)
mergeData$area_floor._m.sqr <- as.numeric(mergeData$area_floor._m.sqr)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)

#str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
merge.sel <- mergeData
#apply(merge.sel,2,function(x) sum(is.na(x)))
## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
#data = cbind(Base_Class,scaled)

train.sample <- scaled[index,]
train.val <- scaled[-index,]  

best.guess <- mean((train.sample$consumption.Kwh.sqm) * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm))
value <- train.val$consumption.Kwh.sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"
results[1,"RMSE.baseline"] <- sqrt(mean((best.guess-value)^2))
results[1,"MAE.baseline"] <- mean(abs(best.guess-value))



## Preparing Neural Network 
#set.seed(2013)
library(neuralnet)
n <- names(train.sample)
f <- as.formula(paste("consumption.Kwh.sqm ~", paste(n[!n %in% c("hour","month","Base_hour_Flag","consumption.Kwh.sqm","Sea_Level_PressureIn","Humidity","VisibilityMPH")], collapse = " + ")))
start.time <- Sys.time()
print(start.time)
nn <- neuralnet(f,data=train.sample,hidden=1,linear.output=T,stepmax = 1e+06)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


#Computing the value would require to pass only the features used in the formula
pr.nn <- neuralnet::compute(nn,train.val[,c("Day.of.Week","weekday","Holiday","area_floor._m.sqr","base_hr_usage","TemperatureF","Dew_PointF","Wind_SpeedMPH","WindDirDegrees")])

#scaling down the predicted results
pr.nn_ <- pr.nn$net.result*(max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

#Scaling down the actual consumptions
test.r <- (train.val$consumption.Kwh.sqm)*(max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

train.val$actual_consumption.Kwh.sqm <- test.r
train.val$predicted_consumption.Kwh.sqm <- pr.nn_

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
#results[1,"Adjusted R Square"] <- summary(lin.reg)$r.squared

write.csv(train.val,"NeuralNetowrk_AllBuildings_Output.csv")
write.csv(results,"All Buildings_NeuralNetwork.csv")


######### KNN CLASSIFICATION

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
str(csv)
#View(csv[which(is.na(csv$TemperatureF)),])
#require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$consumption.Kwh.sqm <- as.numeric(mergeData$consumption.Kwh.sqm)
mergeData$area_floor._m.sqr <- as.numeric(mergeData$area_floor._m.sqr)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)

#str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
merge.sel <- mergeData
#apply(merge.sel,2,function(x) sum(is.na(x)))
## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
#data = cbind(Base_Class,scaled)

train.sample <- scaled[index,]
train.val <- scaled[-index,]  

best.guess <- mean((train.sample$consumption.Kwh.sqm) * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm))
value <- train.val$consumption.Kwh.sqm * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"
results[1,"RMSE.baseline"] <- sqrt(mean((best.guess-value)^2))
results[1,"MAE.baseline"] <- mean(abs(best.guess-value))



## Preparing KNN Regression model 
#set.seed(2013)
library(FNN)

knn.model <- FNN::knn.reg(train=train.sample[,-c(1,2,5,8,12,13,14)],test = train.val[,-c(1,2,5,8,12,13,14)],y=train.sample$consumption.Kwh.sqm,k=5,algorithm = c("brute"))


#scaling down the predicted results
predictedvalues <- knn.model$pred * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

#Scaling down the actual consumptions
test.r <- (train.val$consumption.Kwh.sqm)*(max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

train.val$actual_consumption.Kwh.sqm <- test.r
train.val$predicted_consumption.Kwh.sqm <- pr.nn_

## Evaluating the model's accuracy
resultset <- data.frame(actual = test.r, prediction = predictedvalues)
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
#results[1,"Adjusted R Square"] <- summary(lin.reg)$r.squared

write.csv(train.val,"KNNRegression_AllBuildings_Output.csv")
write.csv(results,"All Buildings_KNNRegression.csv")


## Preparing KNN Regression model 
#set.seed(2013)
library(FNN)

knn.model <- FNN::knn.reg(train=train.sample[,-c(1,2,5,8,12,13,14)],test = train.val[,-c(1,2,5,8,12,13,14)],y=train.sample$consumption.Kwh.sqm,k=5,algorithm = c("brute"))


#scaling down the predicted results
predictedvalues <- knn.model$pred * (max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

#Scaling down the actual consumptions
test.r <- (train.val$consumption.Kwh.sqm)*(max(merge.sel$consumption.Kwh.sqm)-min(merge.sel$consumption.Kwh.sqm))+min(merge.sel$consumption.Kwh.sqm)

train.val$actual_consumption.Kwh.sqm <- test.r
train.val$predicted_consumption.Kwh.sqm <- pr.nn_

## Evaluating the model's accuracy
resultset <- data.frame(actual = test.r, prediction = predictedvalues)
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
#results[1,"Adjusted R Square"] <- summary(lin.reg)$r.squared

write.csv(train.val,"KNNRegression_AllBuildings_Output.csv")
write.csv(results,"All Buildings_KNNRegression.csv")




#########
#########

## KNN CLASSIFICATION

#########
#########

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)
mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
#str(mergeData)
## Split validation
merge.sel <- mergeData

## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
data = cbind(Base_Hour_Class = merge.sel$Base_Hour_Class,scaled)

train.sample <- data[index,]
train.val <- data[-index,]  

### Putting the results into the data frame
results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"

## KNN Regression model
set.seed(2016)
library(class)

## Takes around 20 mins
knn.model <- class::knn(train=train.sample[,-c(1,3,13,14,15)],test = train.val[,-c(1,3,13,14,15)],cl =train.sample$Base_Hour_Class,k=5)



### Takes more than an hour
#library(caret)
#ctrl <- trainControl(method="cv")
#knn.class <- train(Base_Hour_Class ~ hour + consumption.Kwh.sqm + month + Day.of.Week + weekday + Base_hour_Flag + Holiday + base_hr_usage + TemperatureF + Dew_PointF + Wind_SpeedMPH + WindDirDegrees + area_floor._m.sqr,
#                   data= train.sample, 
#                   method = "knn", 
#                   trControl = ctrl,
#                   tuneLength = 1)

## COnfusion matrix
library(caret)
library(e1071)

accuracy <- confusionMatrix(knn.model, train.val$Base_Hour_Class)
results[1,"Accuracy"] <- paste(accuracy$overall[1]*100,"%",sep="")

train.val$Predicted_Base_Hour_Class <- knn.model


## Storing the confusion matrix

tocsv <- data.frame(cbind(t(accuracy$overall),t(accuracy$byClass)))
write.csv(tocsv,file="KNN_allbuildings_ConfusionMatrix.csv")

## Storing the final output
write.csv(train.val,"KNNClassification_AllBuildings_Output.csv")




#########
#########

## Logistic Regression

#########
#########

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)
mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
#str(mergeData)
## Split validation
merge.sel <- mergeData

## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
data = cbind(Base_Hour_Class = merge.sel$Base_Hour_Class,scaled)

train.sample <- data[index,]
train.val <- data[-index,]  

### Putting the results into the data frame
results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"

## Logistic Regression model
set.seed(20190)

## 
n <- names(train.sample)
f <- as.formula(paste("Base_Hour_Class ~", paste(n[!n %in% c("Base_Hour_Class","Holiday","Sea_Level_PressureIn","Humidity","VisibilityMPH")], collapse = " + ")))
start.time <- Sys.time()
logit <- glm(f,family = "binomial",data = train.sample)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

## Predicting the model output
train.val2 <- subset(train.val,select = -c(Base_Hour_Class,Holiday,Humidity,Sea_Level_PressureIn,VisibilityMPH))

predValues <- predict(logit, newdata = train.val2, type = "response")

#Roundig the values
predValues <- sapply(predValues,round,digits=0)
train.val$Base_Hour_Class <- as.numeric(train.val$Base_Hour_Class)-1

## COnfusion matrix
library(caret)
library(e1071)

accuracy <- confusionMatrix(predValues, train.val$Base_Hour_Clas)
results[1,"Accuracy"] <- paste(accuracy$overall[1]*100,"%",sep="")

train.val$Predicted_Base_Hour_Class <- predValues


## Storing the confusion matrix

tocsv <- data.frame(cbind(t(accuracy$overall),t(accuracy$byClass)))
write.csv(tocsv,file="Logistic_allbuildings_ConfusionMatrix.csv")

## Storing the final output
write.csv(train.val,"LogisticRegression_AllBuildings_Output.csv")


#########
#########

## Neural Network Classification

#########
#########

csv <- read.csv("finland_allBuilding_allWeather.csv", stringsAsFactors = FALSE)
require(dplyr)
mergeData <- csv %>% 
  select(-c(X,date,
            vac,BuildingID,
            meternumb,type,Consumption,City, Wind_Direction,Conditions))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)
mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
#merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
#str(mergeData)
## Split validation
merge.sel <- mergeData

## Split validation 75%
index <- sample(1:nrow(merge.sel),round(0.75*nrow(merge.sel)))  

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-10], 2, max) 
mins <- apply(merge.sel[,-10], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-10], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
#Base_Class <- as.numeric(merge.sel$Base_Hour_Class)1
data = cbind(Base_Hour_Class = merge.sel$Base_Hour_Class,scaled)

train.sample <- data[index,]
train.val <- data[-index,]  

### Putting the results into the data frame
results = as.data.frame(NULL)
results[1,"ModelName"] <- "All Buildings"

## Neural network Classification Regression model
set.seed(20190)

n <- names(train.sample)
f <- as.formula(paste("Base_Hour_Class ~", paste(n[!n %in% c("Base_Hour_Class","Holiday","Sea_Level_PressureIn","Humidity","VisibilityMPH")], collapse = " + ")))
start.time <- Sys.time()
nn <- nnet::nnet(f,train.sample, size=3,rang = 0.1,decay = 5e-4, maxit = 350,MaxNWts = 5000)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

## Predicting the model output
train.val2 <- subset(train.val,select = -c(Base_Hour_Class,Holiday,Humidity,Sea_Level_PressureIn,VisibilityMPH))

pr <- predict(nn, train.val2,type = "class")

## COnfusion matrix
library(caret)
library(e1071)

accuracy <- confusionMatrix(pr, train.val$Base_Hour_Clas)
results[1,"Accuracy"] <- paste(accuracy$overall[1]*100,"%",sep="")

train.val$Predicted_Base_Hour_Class <- pr


## Storing the confusion matrix

tocsv <- data.frame(cbind(t(accuracy$overall),t(accuracy$byClass)))
write.csv(tocsv,file="NeuralNetwork_Classification_allbuildings_ConfusionMatrix.csv")

## Storing the final output
write.csv(train.val,"NeuralNetwork_Classification_AllBuildings_Output.csv")

