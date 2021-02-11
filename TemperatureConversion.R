
## Here, we are using an easily available dataset 'airquality'

Rawdata <- airquality


########### Converting temperature units ###########
#
#  Here the temperature is in degree Fahrenheit. Lets convert it to Celsius and vice verse

## Writing a function for temperature conversion

Temperature_UnitConversion = function(Data,CurrentUnit, DesiredUnit){
  Current_Temp = Data
  if(CurrentUnit == 'Celsius' && DesiredUnit == 'Fahrenheit'){
    print("Inputs are correct, proceeding with conversion")
    print("Current Unit is 'Celsius' & Desired Unit is 'Fahrenheit'")
    Converted_Temp = round((Current_Temp * (9/5) + 32),2)
    }else if(CurrentUnit == 'Fahrenheit' && DesiredUnit == 'Celsius'){
      print("Inputs are correct, proceeding with conversion")
      print("Current Unit is 'Fahrenheit' & Desired Unit is 'Celsius'")
      Converted_Temp = round((Current_Temp - 32) * (5/9),2)
      }else{
        print("Please provide units as 'Celsius' or 'Fahrenheit'")
      }
  
  FinalFile = as.data.frame(cbind(Current_Temp,Converted_Temp))
  return(FinalFile)
}


## Checking the results

Results = Temperature_UnitConversion(Rawdata$Temp, 'Fahrenheit', 'Celsius')

