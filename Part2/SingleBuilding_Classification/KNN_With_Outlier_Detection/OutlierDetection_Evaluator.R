knn_model_Outl <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- outlierDetection(csv = name)
    knn_model_Outl <- rbind(knn_model_Outl,modelcoeff)
  }
}

write.csv(knn_model_Outl,"knn_model_Outliers_All.csv")