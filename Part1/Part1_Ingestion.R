
## Reading the raw data 
rawdata <- read.csv("Finland_masked.csv",header = TRUE)
str(rawdata)
head(rawdata)

## Only getting consumption of energy by Electicity and Heating
require(dplyr)
rawdata <- rawdata %>% filter(type %in% c("elect","Dist_Heating"))
str(rawdata)

##convert to Date type and changing int columns to numeric
rawdata$date <- as.Date((strptime(rawdata$date,"%Y%m%d")),"%Y-%d-%m")
rawdata$hour <- as.numeric(rawdata$hour)
rawdata$Consumption <- as.numeric(rawdata$Consumption)
rawdata$vac <- as.character(rawdata$vac)


 
# View(table(rawdata$vac))

###### Adding missing values by judgment
rawdata$vac[which(rawdata$BuildingID == 81909)] <- "Building 27"
rawdata$vac[which(rawdata$BuildingID == 82254)] <- "Building 9"
rawdata$vac[which(rawdata$BuildingID == 83427)] <- "Building 9"
rawdata$vac[which(rawdata$BuildingID == 84681)] <- "Building 9"
#####


## Sum the consumption by taking  readings from all the meters for each type of each Building ID
require(dplyr)
rawdata.agg <- rawdata %>%  select(BuildingID,vac,meternumb,type,date,hour,Consumption) %>% group_by(BuildingID,meternumb,vac,type,date,hour) %>% 
  summarise(Consumption = sum(Consumption))

#View(rawdata.agg[which(rawdata.agg$vac == "Building 6" & rawdata$meternumb == 1 & rawdata.agg$date == "2013-01-01"),])  

#write.csv(rawdata.agg,"rawtestagg.csv")
## It is found that most of the building had observations of around 8208 or 16416. Which seems to be right given that we have 342 days and 24 hours with 2 types of electicity possible.
## 342*24 or 342*24*2. Somwhere it is observed less than these ranges, however close observation reveals that for those of building, for e.g. Building 4 and Building 13, it comes out that initially they had 2 types of reading and then only one type. 
## So overall, it can be concluded that there were no missing records
View(table(rawdata.agg$vac))

## Adding features from the existing data


##Function to find the Weekday
checkWeekday <- function(date){
  wday.r = if((wday(date) == 6 || wday(date) == 7)) 0 else 1
  return(wday.r)
}

baseHour <- function(hour){
  hour.r = if(hour %in% c(0,1,2,3,4,22,23)) 1 else 0
  return(hour.r)
}

require(lubridate)
require(dplyr)
finlandEnergy.agg <- rawdata.agg %>% mutate(month = month(date),Day.of.Week = wday(date)) %>%
  mutate(weekday = sapply(date, function(x) checkWeekday(x))) %>%
  mutate(Base_hour_Flag = sapply(hour, function(x) baseHour(x)))

## Need to make function of this
require(RCurl)
require(XML)
country <- "finland"
year <- 2013
url <- paste("http://www.timeanddate.com/holidays/",country,"/",year,sep = "")
SOURCE <-getURL(url,encoding = "UTF-8")
PARSED <- htmlParse(SOURCE)
holidays <- xpathSApply(PARSED, "//th[@class='nw']",xmlValue)
holidaydates <- paste(holidays,year,sep = ",")
holidaydatescon <- as.Date(holidaydates,format = "%B %d,%Y")

checkHoliday <- function(date){
 ## Call the function here to get holidaydates
 hol = if(date %in% holidaydatescon) 1 else 0
 return(hol)
}

require(dplyr)
finlandEnergy.agg <- finlandEnergy.agg %>% mutate(Holiday = sapply(date, function(x) checkHoliday(x)))

## reading the "Area" file

area.csv <- read.csv("Finland_addresses_area.csv",header = TRUE,stringsAsFactors = FALSE)
area.csv$area_floor._m.sqr <- as.numeric(area.csv$area_floor._m.sqr)


####### Function to get Airport city by sending Address
source("GeoCode.R") ## This functions returns latitude and longitude for any address in a character space seperated format
address.geocode <- area.csv[1:10,] %>% mutate(LatLong = sapply(X..address, function(x) getGeoCode(x)))
address.geocode <- area.csv %>% mutate(LatLong = sapply(X..address, function(x) getGeoCode(x)))

require(dplyr)
require(tidyr)
address.geocode$LatLong <- as.character(address.geocode$LatLong)

address.geocode <- address.geocode %>%  separate(LatLong,c("latitude","longitude")," ")
address.geocode$latitude <- as.numeric(address.geocode$latitude)
address.geocode$longitude <- as.numeric(address.geocode$longitude)

## Now we can get the nearest city to this airport using the function attached
source("AirportFunction.R") 
#address.final <- address.geocode %>% mutate(City = sapply(latitude,longitude, function(x,y) getAirpot(x,y)))
address.geocode$City <- mapply(getAirpot,address.geocode$latitude,address.geocode$longitude)

## Get Weather Data for the Airport Code
stationcode <- as.matrix(unique(address.geocode$City))


##Get WeatherData for each airport code
source("WeatherFunction.R")
for(i in stationcode){
  df <- getWeatherData(i,mindate = min(finlandEnergy.agg$date),maxdate = max(finlandEnergy.agg$date))
  assign(i,df)
}


## Merge data
mergeData <- merge(finlandEnergy.agg,address.geocode,by.x = "vac",by.y = "building")


## Normalizing Power consumption by area of building
require(dplyr)
finlandEnergy <- mergeData %>% mutate("consumption Kwh/sqm" = Consumption/area_floor._m.sqr)
finlandEnergy <- finlandEnergy %>% select(-c(X..address,latitude,longitude))
finlandEnergy$City <- as.character(finlandEnergy$City)


## Adding column base_hr_usage and Base_Hour_class
finlandEnergy <- finlandEnergy %>% group_by(BuildingID,meternumb,vac,type,date) %>% 
  mutate(base_hr_usage = mean(Consumption[hour %in% c(0,1,2,23)]))

finlandEnergy <- mutate(finlandEnergy,Base_Hour_Class = ifelse(Consumption > base_hr_usage,"High","Low"))

## Spitting the finlandEnergy data with individual buildings
Buildinglist <- split(finlandEnergy, with(finlandEnergy, interaction(BuildingID,meternumb)), drop = TRUE)
#lapply(names(Buildinglist), function(name) write.csv(Buildinglist[[name]], file = paste('Buildings/',gsub(' ','',name),sep ='',".csv"), row.names = F))


#### Merging Weather related data with individual building type based on there location
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  #value <- (strsplit(names(Buildinglist[m]),split = " ")[[1]])[2]
  if(unique(i["City"]) == "EFHK"){
    df <- merge(i,EFHK,by = c("date","hour"))
    name <- paste(build,".",value,sep = "")
    assign(name,df)
  } else if(unique(i["City"]) == "EFHF"){
    df <- merge(i,EFHF,by = c("date","hour"))
    name <- paste(build,".",value,sep = "")
    assign(name,df)
  }
}


##### Combining the Overall dataset with weather
finland_Build_Weather = as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    temp <- get(paste(build,".",value,sep = ""))
    finland_Build_Weather <- rbind(finland_Build_Weather,temp)
  }
}


write.csv(finland_Build_Weather,"finland_allBuilding_allWeather.csv")
