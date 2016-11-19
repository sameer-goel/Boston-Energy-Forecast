
### Evaluating logistic Regression on 78 different datasets
source("Part3_LogisticClassification.R")
Logistic <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnLogistic(csv = name)
    Logistic <- rbind(Logistic,modelcoeff)
  }
}
write.csv(Logistic,"logisticregression_All1.csv")
