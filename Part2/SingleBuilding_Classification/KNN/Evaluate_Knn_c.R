
### For Loop -- KNN Classification for 78 model
knn_model_c <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnKnnClass(csv = name)
    knn_model_c <- rbind(knn_model_c,modelcoeff)
  }
}

write.csv(knn_model_c,"knn_model_c_All.csv")
