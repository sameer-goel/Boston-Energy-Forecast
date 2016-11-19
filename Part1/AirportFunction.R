getAirpot <- function(lat,long){
  #272f5ee91491de50
  #3a3f6a4875014282
  root <- "http://api.wunderground.com/api/3a3f6a4875014282/geolookup/q/"
  u <- paste0(root,lat,",",long,".json")
  require(RJSONIO)
  target <- URLencode(u)
  dat <- fromJSON(target)
  airports <- dat$location$nearby_weather_stations$airport$station
  q = NULL
  for (i in 1:length(airports)){
      q <- rbind(q,airports[[i]])
  }
  
  q <- as.data.frame(q,stringsAsFactors = FALSE)
  q[,"distance"] <- NA
  for (i in 1:nrow(q)){
    if(q[i,"city"] != "" & q[i,"icao"] != ""){
      q[i,"distance"] <- getdistance(lat,long,as.numeric(q[i,"lat"]),as.numeric(q[i,"lon"]))
    }
  }
  
  city <- q[which.min(q$distance),]["icao"]
  return(city)
  
}

## Get distance between two points on Earth. Do not try to understand it!
getdistance <- function (lat1, lon1, lat2, lon2) {
   p = 0.017453292519943295    # Math.PI / 180
   a = 0.5 - cos((lat2 - lat1) * p)/2 + cos(lat1 * p) * cos(lat2 * p) * (1 - cos((lon2 - lon1) * p))/2
  
  return(12742 * asin(sqrt(a))) ## 2 * R; R = 6371 km
}