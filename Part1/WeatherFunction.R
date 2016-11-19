getWeatherData <- function(statcode,mindate,maxdate){
  
  if(is.na(statcode) | is.na(mindate) | is.na(maxdate)){
    print("Values supplied should not be null")
    return()
  }
  
  ## Lets get the station code
  require(weatherData)
  code <- getStationCode(statcode)
  if (is.null(code) | length(code) == 0){
    print("Station code does not exist!")
    return()
  }
 
  WeatherData <- getWeatherForDate(statcode, start_date=mindate,
                                   end_date = maxdate,
                                   opt_detailed=T,opt_custom_columns=T,
                                   custom_columns=c(2:13))
  
  library(lubridate)
  
  WeatherData$date = date(WeatherData$Time)
  WeatherData$hour = hour(WeatherData$Time)
  
  # Removing data for which dates are NA
  WeatherData <- subset(WeatherData,(!is.na(WeatherData$date)))
  
  
  WeatherData$date <-  as.Date(WeatherData$date,"%m/%d/%Y")
  WeatherData$TemperatureF <- as.numeric(WeatherData$TemperatureF)
  WeatherData$Dew_PointF <- as.numeric(WeatherData$Dew_PointF)
  WeatherData$Sea_Level_PressureIn <- as.numeric(WeatherData$Sea_Level_PressureIn)
  WeatherData$VisibilityMPH <- as.numeric(WeatherData$VisibilityMPH)
  WeatherData$WindDirDegrees <- as.numeric(WeatherData$WindDirDegrees)
  WeatherData$Humidity <- as.numeric(WeatherData$Humidity)
  
  WeatherData$Wind_SpeedMPH[WeatherData$Wind_SpeedMPH == "Calm"] <- 0
  WeatherData$Wind_SpeedMPH <- as.numeric(WeatherData$Wind_SpeedMPH)
  
  
  
  ## We need our data to fall in normal range to remove outliers
  ##                        MIN   MAX
  ##    TempF	              -60	  120
  ##    DewPointF	          -60	  120
  ##    Humidity	           10	  100
  ##    Sea_Level_Pressure	 28	  32
  ##    Visibility	          0   20
  ##    Wind_Speed	          0 	50
  
  #### Function to relace oulier value Attached. We will use the approach of subsituting the previous or the next value of the observation
  ## For example, if the record 8999 has Temperature as -9999 we will use the record of 8998 so that this is still acceptable
  
  ## Tell something about the function
  
  remove_out <- function(param,index,min_v,max_v){
    val = NULL
    val = param[index]
    if(val < min_v | val > max_v | is.na(val)){
      if(index-1 >= 1){
        val = param[index-1]
      } else if (index-1 <= 0){
        val = param[index+1]
      } 
      return(val)
    } else{
      print("Nothing changed")
      return(val)  #Normal Value return
    }
  }
  
  ## Removing outliers/missing values using the above function from all the features
  
  ## Temperature
  index <- which(WeatherData$TemperatureF < -60 | WeatherData$TemperatureF > 120 | is.na(WeatherData$Dew_PointF))
  
  for (i in index){
    WeatherData$TemperatureF[i] = remove_out(WeatherData$TemperatureF,i,-60,120)
  }
  
  ## Dew Point
  index <- which(WeatherData$Dew_PointF < -60 | WeatherData$Dew_PointF > 120 | is.na(WeatherData$Dew_PointF))
  for (i in index){
    WeatherData$Dew_PointF[i] = remove_out(WeatherData$Dew_PointF,i,-60,120)
  }
  
  ## Humidity
  index <- which(WeatherData$Humidity < 0 | WeatherData$Humidity > 100 | is.na(WeatherData$Humidity))
  for (i in index){
    WeatherData$Humidity[i] = remove_out(WeatherData$Humidity,i,0,100)
  }
  
  ## Wind_SpeedMPH
  index <- which(WeatherData$Wind_SpeedMPH < 0 | WeatherData$Wind_SpeedMPH > 50 | is.na(WeatherData$Wind_SpeedMPH))
  for (i in index){
    WeatherData$Wind_SpeedMPH[i] = remove_out(WeatherData$Wind_SpeedMPH,i,0,50)
  }
  
  ## Sea_Level_Pressure
  index <- which(WeatherData$Sea_Level_PressureIn < 28 | WeatherData$Sea_Level_PressureIn > 32 | is.na(WeatherData$Sea_Level_PressureIn))
  for (i in index){
    WeatherData$Sea_Level_PressureIn[i] = remove_out(WeatherData$Sea_Level_PressureIn,i,28,32)
  }
  
  ## VisibilityMPH
  index <- which(WeatherData$VisibilityMPH < 0 | WeatherData$VisibilityMPH > 20 | is.na(WeatherData$VisibilityMPH))
  for (i in index){
    WeatherData$VisibilityMPH[i] = remove_out(WeatherData$VisibilityMPH,i,0,20)
  }
  
  ## WindDirDegree
  index <- which(WeatherData$WindDirDegrees < 0 | WeatherData$WindDirDegrees > 360 | is.na(WeatherData$WindDirDegrees))
  for (i in index){
    WeatherData$WindDirDegrees[i] = remove_out(WeatherData$WindDirDegrees,i,0,360)
  }
  
  ## Now since the data is clean, we can aggregate the dataset as done in part 1, so that we get records for each hour and we can take average values for numeric values and frequency count for character values
  ## Steps involved 1) Remove non essential features like Time, Gust_speedMH,P,E
  ## 2) Group the data by Date and hour 
  ## 3) summarise base on mean and frequency count
  
  #View(WeatherData.Agg[which(WeatherData.Agg$date == "2013-10-27"),])
  
  require(dplyr)
  WeatherData.Agg <- WeatherData %>% select(-c(Time,Gust_SpeedMPH,PrecipitationIn)) %>% group_by(date,hour) %>% 
    summarise(TemperatureF = mean(TemperatureF),
              Dew_PointF = mean(Dew_PointF),
              Humidity = mean(Humidity),
              Sea_Level_PressureIn = mean(Sea_Level_PressureIn),
              VisibilityMPH = mean (VisibilityMPH),
              Wind_SpeedMPH = mean(Wind_SpeedMPH),
              WindDirDegrees = mean(WindDirDegrees),
              Conditions = names(table(Conditions))[which.max(table(Conditions))],
              Wind_Direction = names(table(Wind_Direction))[which.max(table(Wind_Direction))])
  
  
  ## Fill missing series of time
  require(dplyr)
  timeseries <- data.frame(DateTime = seq.POSIXt(as.POSIXlt(mindate),as.POSIXlt(maxdate+1)-1 ,by="hour"))
  timeseries$hour <- as.integer(stringr::str_sub(timeseries$DateTime, -8, -7))
  timeseries$date <- as.Date(strftime(timeseries$DateTime,format = "%Y-%m-%d %H:%M:%S",tz = "UTC"),"%Y-%m-%d")
  timeseries <- subset(timeseries,select = -c(DateTime))
  
  
  finalWeather <- merge(WeatherData.Agg,timeseries,by = c("date","hour"),all.y = TRUE)
  
  #View(finalWeather[which(is.na(finalWeather$VisibilityMPH)),])
  ##Padding function
  remove_padd <- function(param,index){
    val = NULL
    val = param[index]
    if(index-1 >= 1){
      val = param[index-1]
    } else if (index-1 <= 0){
      val = param[index+1]
    } else {
      print("Nothing changed")
      return(val)  #Normal Value return
    }
    return(val)
  }
  
  
  index <- which(is.na(finalWeather$TemperatureF))
  for (i in index){
    finalWeather$TemperatureF[i] = remove_padd(finalWeather$TemperatureF,i)
    finalWeather$Dew_PointF[i] = remove_padd(finalWeather$Dew_PointF,i)
    finalWeather$Humidity[i] = remove_padd(finalWeather$Humidity,i)
    finalWeather$Sea_Level_PressureIn[i] = remove_padd(finalWeather$Sea_Level_PressureIn,i)
    finalWeather$VisibilityMPH[i] = remove_padd(finalWeather$VisibilityMPH,i)
    finalWeather$Wind_SpeedMPH[i] = remove_padd(finalWeather$Wind_SpeedMPH,i)
    finalWeather$WindDirDegrees[i] = remove_padd(finalWeather$WindDirDegrees,i)
    finalWeather$Conditions[i] = remove_padd(finalWeather$Conditions,i)
    finalWeather$Wind_Direction[i] = remove_padd(finalWeather$Wind_Direction,i)
  }
  
  
  return(finalWeather)
  
}