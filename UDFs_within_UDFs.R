
## Here, we are using an easily available dataset 'airquality'

Rawdata <- airquality

## Creating Date Column
  
Rawdata$Date <- as.Date(paste0("2020-",Rawdata$Month,"-", Rawdata$Day))

########### Converting temperature units ###########
#
#  Here the temperature is in degree Fahrenheit. Lets convet it to Celsius
#       T(°C) = (T(°F) - 32) × 5/9
#
###########

Temperature_UnitConversion = function(Data,CurrentUnit, DesiredUnit){
  Current_Temp = Data
  
      if(CurrentUnit == 'Celsius' && DesiredUnit == 'Fahrenheit'){
        # print("Inputs are correct, proceeding with conversion")
        # print("Current Unit is 'Celsius' & Desired Unit is 'Fahrenheit'")
        Converted_Temp = round((Current_Temp * (9/5) + 32),2)
      } else if(CurrentUnit == 'Fahrenheit' && DesiredUnit == 'Celsius'){
        # print("Inputs are correct, proceeding with conversion")
        # print("Current Unit is 'Fahrenheit' & Desired Unit is 'Celsius'")
        Converted_Temp = round((Current_Temp - 32) * (5/9),2)
      } else{
        print("Please provide units as 'Celsius' or 'Fahrenheit'")
      }
  
  FinalFile = as.data.frame(cbind(Current_Temp,Converted_Temp))

  return(FinalFile)
  }

Results = Temperature_UnitConversion(Rawdata$Temp, 'Fahrenheit', 'Celsius')


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
    MaxTemp_inCelsius = max(Temperature_UnitConversion(MonthlyData$Temp,'Fahrenheit', 'Celsius')[,"Converted_Temp"])
    MinTemp_inCelsius = min(Temperature_UnitConversion(MonthlyData$Temp,'Fahrenheit', 'Celsius')[,"Converted_Temp"])
    
    MonthInformation = cbind(MonthName = i,NoofDays, AverageWindSpeed, MaxTemp_inCelsius, MinTemp_inCelsius)
    FinalDataset = as.data.frame(rbind(FinalDataset,MonthInformation))
  }
  return(FinalDataset)
}

## Running the final nested UDF

MonthlyDetails = MonthlyAirQualityDetails(Rawdata)

