
## Here, we are using an easily available dataset 'airquality'

Rawdata <- airquality

## Creating Date Column
  
Rawdata$Date <- as.Date(paste0("2020-",Rawdata$Month,"-", Rawdata$Day))

########### Getting Monthly Data ###########
#
#  Here we are planning to get various information at a monthly level
#
###########


MonthlyAirQualityDetails = function(Dataset){
  FinalDataset = NULL
  
  for(i in unique(months(Dataset$Date))){
    
    MonthlyData = Rawdata[which(months(Dataset$Date) == i),]
    
    NoofDays = nrow(MonthlyData)
    AverageWindSpeed = round(mean(MonthlyData$Wind),2)
    MaxTemp = max(MonthlyData$Temp)
    MinTemp = min(MonthlyData$Temp)
    
    MonthInformation = cbind(MonthName = i,NoofDays, AverageWindSpeed, MaxTemp, MinTemp)
    FinalDataset = as.data.frame(rbind(FinalDataset,MonthInformation))
    }
  return(FinalDataset)
}

MonthlyDetails = MonthlyAirQualityDetails(Rawdata)

