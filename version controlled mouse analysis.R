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
library(dplyr)
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
monthly<-initial_data$Start.month

#Some simple plots for each predictor 
install.packages("ggplot2")
install.packages("patchwork")
library(patchwork)
library(ggplot2)
smellplot<-ggplot(initial_data,aes(Timesincedeath,simplescent))+
  geom_point(aes(x=Timesincedeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE) +
  labs(x="time since death (min)", y ="presence of smell",
       title = "D")
furplot<-ggplot(initial_data,aes(Timesincedeath,furtime))+
  geom_point(aes(x=Timesincedeath))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="fur status",
       title = "C")
infestplot<-ggplot(initial_data,aes(Deathtimer,InfestStatus))+
  geom_point(aes(x=Deathtimer))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="infestation status",
       title = "B")
eyeplot<-ggplot(initial_data,aes(Deathtimer,Eyestatus))+
  geom_point(aes(x=Deathtimer))+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)+
  labs(x="time since death (min)", y ="eye status",
       title = "A")
basicplot1<-eyeplot+infestplot+furplot+smellplot
basicplot1<-basicplot1+plot_annotation(title="Carcass characteristics against time since death")
#A = eyes, B = infestation, C - fur, D = scent
basicplot1

#Now, we are going to build some GLMs and remove statistically insignificant variables
install.packages("TMB",type = 'source')
library(TMB)
install.packages("glmmTMB")
library(glmmTMB)

#Determining statistically significant predictors for smell model
smellGLM<-glmmTMB(Deathtimer~simplescent*monthly+(1|ID)+Mousecut,data=initial_data)
summary(smellGLM)
#month and interaction term not significant. Removing along with treatment
smellGLM2<-glmmTMB(Deathtimer~simplescent+(1|ID),data=initial_data)
summary(smellGLM2)

#Determining statistically significant predictors for fur model 
furGLM<-glmmTMB(Deathtimer~furtime*monthly+(1|ID)+Mousecut,data=initial_data)
summary(furGLM)
#Treatment not significant
furGLM2<-glmmTMB(Deathtimer~furtime*monthly+(1|ID),data=initial_data)
summary(furGLM2)

#Determining statistically significant predictors for infestation model
InfestGLM<-glmmTMB(Deathtimer~InfestStatus*monthly+(1|ID)+Mousecut,data=initial_data)
summary(InfestGLM)
#Treatment not significant
InfestGLM2<-glmmTMB(Deathtimer~InfestStatus*monthly+(1|ID),data=initial_data)
summary(InfestGLM2)

#Determining statistically significant predictors for eye model
EyeGLM<-glmmTMB(Deathtimer~Eyestatus*monthly+(1|ID)+Mousecut,data=initial_data)
summary(EyeGLM)
#Month and treatment not significant, along with interaction term
EyeGLM2<-glmmTMB(Deathtimer~Eyestatus+(1|ID),data=initial_data)
summary(EyeGLM2)

#Fitting a model for all predictors
InitialGLM<-glmmTMB(Deathtimer~Eyestatus+InfestStatus+furtime+simplescent + monthly + (1|ID),data=initial_data, family = Gamma(link = "log"))
summary(InitialGLM)
#Month not significant, removed 
InitialGLM2<-glmmTMB(Deathtimer~Eyestatus+InfestStatus+furtime+simplescent + (1|ID),data=initial_data, family = Gamma(link = "log"))
summary(InitialGLM2)

#Comparing AIC and log-likelihood for various combinations of predictors
#Begin by making a test for each combination 

test1<-glmmTMB(Deathtimer~Eyestatus+furtime+simplescent+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test1)
test2<-glmmTMB(Deathtimer~Eyestatus+InfestStatus+simplescent+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test2)
test3<-glmmTMB(Deathtimer~Eyestatus+InfestStatus+furtime+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test3)
test4<-glmmTMB(Deathtimer~simplescent+InfestStatus+furtime+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test4)
test5<-glmmTMB(Deathtimer~simplescent+furtime+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test5)
test6<-glmmTMB(Deathtimer~Eyestatus+InfestStatus+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test6)
test7<-glmmTMB(Deathtimer~furtime+InfestStatus+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test7)
test8<-glmmTMB(Deathtimer~simplescent+Eyestatus+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test8)
test9<-glmmTMB(Deathtimer~furtime+InfestStatus+Eyestatus+simplescent+(1|ID),data=initial_data,family=Gamma(link="log"))
summary(test9)

#Diagnostics for each test- are we meeting assumptions?
install.packages("DHARMa")
library(DHARMa)
simulation1output<-simulateResiduals(fittedModel=test1,plot=TRUE)
simulation2output<-simulateResiduals(fittedModel=test2,plot=TRUE)
simulation3output<-simulateResiduals(fittedModel=test3,plot=TRUE)
simulation4output<-simulateResiduals(fittedModel=test4,plot=TRUE)
simulation5output<-simulateResiduals(fittedModel=test5,plot=TRUE)
simulation6output<-simulateResiduals(fittedModel=test6,plot=TRUE)
simulation7output<-simulateResiduals(fittedModel=test7,plot=TRUE)
simulation8output<-simulateResiduals(fittedModel=test8,plot=TRUE)
simulation9output<-simulateResiduals(fittedModel=test9,plot=TRUE)
logLik(test1)
logLik(test2)
logLik(test3)
logLik(test4)
logLik(test5)
logLik(test6)
logLik(test7)
logLik(test8)
logLik(test9)

#Test9 is our best model, it includes all the predictors
#Time to make a plot that visualizes these 
install.packages("ggeffects")
library(ggeffects)
marginal1<-ggpredict(test9, terms = c("InfestStatus","furtime","simplescent","Eyestatus"))
plot(marginal1)
colnames(marginal1)<-c('infest', 'pred', 'se', 'cl', 'cu', 'fur', 'scent', 'eyes')
SecondFigure<-ggplot(data=marginal1, aes(x=as.factor(infest), y=pred, ymin=cl, ymax=cu,color=as.factor(eyes)))+geom_pointrange()+facet_grid(fur~scent)
SecondFigure
SecondFigure2<-SecondFigure+labs(title="Model predictions for time since death")
furnames<-c('0'="Intact",'1'="Removable")
scentnames<-c('0'="No scent",'1'="scent present") 
SecondFigure3<-SecondFigure2+facet_grid(fur~scent,labeller=labeller(fur=as_labeller(furnames),scent=as_labeller(scentnames)))
SecondFigure4<-SecondFigure3+scale_color_discrete(name = "Eyes", labels = c("1" = "Damaged/Absent", "0" = "Round and moist"))
SecondFigure5<-SecondFigure4+labs(x = "Infestation", y = "Time since death (min)")
SecondFigure5

install.packages("writexl")
library(writexl)
tempfile(fileext = "C:\\Users\\nicho\\Documents\\001 Nicholas Documents\\Employment\\AAA BCI\\mouse paper\\test.xlsx")
write_xlsx(marginal1)
