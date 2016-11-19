### Getting Laitude and longitude data

getGeoCode <- function(address,sensor = "false") {
  require(RJSONIO)
  root <- "http://maps.google.com/maps/api/geocode/json?"
  u <- paste0(root,"address=", address, "&sensor=false@key=AIzaSyBmO7kN8BXjz_prZnTFauynxpr4eHsADosA")
  target <- URLencode(u)
  dat <- fromJSON(target)
  if(length(dat$results) == 0){
    print("No Such Address found")
    return()
  }
  
  latitude <- dat$results[[1]]$geometry$location["lat"]
  longitude <- dat$results[[1]]$geometry$location["lng"]
  latlong <- paste(latitude,longitude,sep = " ")
  latlong <- as.character(latlong)
  return(latlong)
}