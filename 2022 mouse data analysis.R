#2022 mouse data analysis

#Load initial Survey123 data
#CarcassInfo1 will be the data for each specific check
#CarcassInfo2 will be the data for each mouse overall
CarcassInfo1 <- read.csv("C:\\Users\\nicho\\Documents\\001 Nicholas Documents\\Employment\\AAA BCI\\mouse paper\\CarcassInfo1_1.csv")
View(CarcassInfo1)
CarcassInfo2 <- read.csv("C:\\Users\\nicho\\Documents\\001 Nicholas Documents\\Employment\\AAA BCI\\mouse paper\\Mouse_Survey_2022_0.csv")
View(CarcassInfo2)

#Need to make a time since death column
#take time of check (CarcassInfo1) and
#then subtract from time of death(CarcassInfo2)
#Rename column in CarcassID2
colnames(CarcassInfo2)[2]="ParentGlobalID"
#remove Creation Date from CarcassInfo1 to prevent confusion
install.packages("dplyr")
library(dplyr)
CarcassInfo1<-select(CarcassInfo1,-13)
#rename workup time to make things easier
colnames(CarcassInfo1)[4]="CheckTime"
#Then merge the two data frames by that column
mydata<-merge(CarcassInfo1,CarcassInfo2,by="ParentGlobalID")
#need date-time objects for difftime function
Checktimes<-as.POSIXct(mydata$`CheckTime`, format = "%m/%d/%Y %H:%M:%S")
Starttimes<-as.POSIXct(mydata$`CreationDate`, format = "%m/%d/%Y %H:%M:%S")
#Need to set time zone for difftime function
install.packages("lubridate")
library(lubridate)
Starttimes2<-with_tz(Starttimes,"UTC")
Checktimes2<-with_tz(Checktimes,"UTC")
#run the difftime function
TimeSinceDeath<-difftime(Checktimes,Starttimes,units="mins")
TimeSinceDeath
TimeSinceDeath<-round(TimeSinceDeath,0)
TimeSinceDeath
#Then, filter out test entries and errors
mydata[mydata$CreationDate,43]
mydata$TimeSinceDeath<-TimeSinceDeath
mydata<-filter(mydata,TimeSinceDeath>0)
mydata$TimeSinceDeath
mydata<-filter(mydata,TimeSinceDeath<10000)
mydata$TimeSinceDeath
Deathtimer<-as.numeric(TimeSinceDeath)
#Our outcome variable is now ready for analysis
#Now we build our predictor variables 
#Scent, Infestation, Eyes, Fur
scent<-ifelse(mydata$Scent.of.Decay.=="None",0,1)
#will make one column for both eyes
eyes<-ifelse(mydata$Left.Eye.Condition=="RoundFluid"&mydata$Right.Eye.Condition=="RoundFluid",0,1)
fur<-ifelse(mydata$Fur.Condition=="Removable",1,0)
infest<-ifelse(mydata$Infestation=="None",0,1)

install.packages("ggplot2")
install.packages("patchwork")
library(patchwork)
library(ggplot2)
smellplot<-ggplot(mydata,aes(TimeSinceDeath,scent))+
  geom_point(aes(x=TimeSinceDeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x="time since death (min)", y ="presence of smell",
       title = "D")
furplot<-ggplot(mydata,aes(TimeSinceDeath,fur))+
  geom_point(aes(x=TimeSinceDeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="fur status",
       title = "C")
infestplot<-ggplot(mydata,aes(TimeSinceDeath,infest))+
  geom_point(aes(x=TimeSinceDeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="infestation status",
       title = "B")
eyeplot<-ggplot(mydata,aes(TimeSinceDeath,eyes))+
  geom_point(aes(x=TimeSinceDeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="eye status",
       title = "A")
basicplot1<-eyeplot+infestplot+furplot+smellplot
basicplot1<-basicplot1+plot_annotation(title="Carcass characteristics against time since death")
basicplot1
