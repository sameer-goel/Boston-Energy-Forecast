############## PREDICTION on continous variable


### Evaluating Linear Model on 78 different datasets
source("Part1_LinearRegression")
linearmodel <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnLinearModel(csv = name)
    linearmodel <- rbind(linearmodel,modelcoeff)
  }
}
write.csv(linearmodel,"linearModel_All1.csv")

### Evaluating Random forest Model on 78 different datasets
source("Part2_RandomForest.R")
randomForest <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnRandomForest(csv = name,ntree = 100)
    randomForest <- rbind(randomForest,modelcoeff)
  }
}

write.csv(randomForest,"radomForest_All.csv")


### Evaluating Neural Network Model on 78 different datasets
source("Part2_NeuralNetwork.R")
neuralnetwork <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnNeuralNetwork(csv = name)
    neuralnetwork <- rbind(neuralnetwork,modelcoeff)
  }
}

write.csv(neuralnetwork,"radomForest_All.csv")


### Evaluating Knn Regression Model on 78 different datasets
source("Part2_KNN.R")
knnmodel <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnKNN(csv = name,k=3)
    knnmodel <- rbind(knnmodel,modelcoeff)
  }
}

write.csv(knnmodel,"knnResults_All.csv")



### Evaluating Neural Network classification on 78 different datasets
source("Part2_NeuralNetwork_Classification.R")
neuralnetworkclass <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnNeuralNetworkclass(csv = name)
    neuralnetworkclass <- rbind(neuralnetworkclass,modelcoeff)
  }
}

write.csv(neuralnetwork,"radomForest_All.csv")


