

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

write.csv(neuralnetworkclass,"NeuralNetwork_Classification_All.csv")


