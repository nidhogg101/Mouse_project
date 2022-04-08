#Nicholas Boulanger
#2021 Iowa Wind Study
#Mouse Decomposition Study

#Load initial data
install.packages("readxl")
library(readxl)
initial_data <- read_excel("C:\\Users\\nicho\\Documents\\001 Nicholas Documents\\Employment\\AAA BCI\\mouse paper\\initial data.xlsx")
View(initial_data)

#Trying to generate a "time since death" column 
#Essentially a subtraction from check time to start
#Start by putting the date & time column in POSIXct format
Checktimes<-as.POSIXct(initial_data$`Date_&_Time`, format = "%m/%d/%Y %H:%M:%S")
Checktimes

# Then establish time zone 
install.packages("lubridate")
library(lubridate)
Checktimes2<-with_tz(Checktimes,"UTC")

#Start time classification, specifying format as above
Starttimes<-as.POSIXct(initial_data$`Start_date_&_time`, format = "%m/%d/%Y %H:%M:%S")
Starttimes2<-with_tz(Starttimes,"UTC")

#Building a column with the calculation 
TimeSinceDeath<-difftime(Checktimes2,Starttimes2,units="mins")
TimeSinceDeath<-round(TimeSinceDeath,0)
initial_data$Timesincedeath<-TimeSinceDeath
Deathtimer<-as.numeric(initial_data$Timesincedeath)
#Our outcome variable is now ready for analysis
#Now we build our predictor variables 

