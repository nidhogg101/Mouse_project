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
#Scent, Infestation, Eyes, Fur
#Also, ID, cut treatment, and month
simplescent<-ifelse(initial_data$Smell=="None",0,1)
Eyestatus<-ifelse(initial_data$Eyes=="glossy",0,1)
furtime<-ifelse(initial_data$'Fur'=="Removable",1,0)
#Need to make a column for 'total' infestation - maggots & everything else 
install.packages("dplyr")
library(dplr)
initial_data<-mutate(initial_data,Infestation_status = case_when(initial_data$`Maggots Present?`=="No"&initial_data$`Other Infestation`=="None"~ "No",
                                             initial_data$`Maggots Present?`=="No"&!initial_data$`Other Infestation`=="None"~"Yes",
                                             !initial_data$`Maggots Present?`=="No"&initial_data$`Other Infestation`=="None"~"Yes",
                                             
                                             !initial_data$`Maggots Present?`=="No"&!initial_data$`Other Infestation`=="None"~"Yes"
))
InfestStatus<-ifelse(initial_data$Infestation_status=="Yes",1,0)
#creating Mouse ID 
initial_data<-transform(initial_data,ID=as.numeric(interaction(initial_data$`Start month`,initial_data$Turbine,initial_data$Flag,drop=TRUE)))
Mousecut<-ifelse(initial_data$TREATMENT=="Cut",1,0)
Month1<-ifelse(initial_data$Start.month=="August",1,0)

#Some simple plots for each predictor 
install.packages("ggplot2")
install.packages("patchwork")
library(patchwork)
library(ggplot2)
smellplot<-ggplot(initial_data,aes(Timesincedeath,simplescent))+
  geom_point(aes(x=Timesincedeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x="time since death (min)", y ="presence of smell",
       title = "decay smell vs carcass age")
furplot<-ggplot(initial_data,aes(Timesincedeath,furtime))+
  geom_point(aes(x=Timesincedeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="fur status",
       title = "condition of fur vs carcass age")
infestplot<-ggplot(initial_data,aes(Deathtimer,InfestStatus))+
  geom_point(aes(x=Deathtimer))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="infestation status",
       title = "infestation vs carcass age")
eyeplot<-ggplot(initial_data,aes(Deathtimer,Eyestatus))+
  geom_point(aes(x=Deathtimer))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="eye status",
       title = "eye status vs carcass age")
basicplot1<-eyeplot+infestplot+furplot+smellplot
basicplot1<-basicplot1+plot_annotation(title="Carcass characteristics against time since death")
