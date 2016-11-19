rf_model_c <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    print(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnRandomForest(csv = name)
    rf_model_c <- rbind(rf_model_c,modelcoeff)
  }
}
write.table(rf_model_c,"rf_model_c_All.csv", sep = "," )
write.csv(rf_model_c,"rf_model_c_All1.csv")
