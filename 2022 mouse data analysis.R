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
mydata$scent<-ifelse(mydata$Scent.of.Decay.=="None",0,1)
#will make one column for both eyes
mydata$eyes<-ifelse(mydata$Left.Eye.Condition=="RoundFluid"&mydata$Right.Eye.Condition=="RoundFluid",0,1)
mydata$fur<-ifelse(mydata$Fur.Condition=="Removable",1,0)
mydata$infest<-ifelse(mydata$Infestation=="None",0,1)

#some simple plots for each predictor
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

install.packages("TMB",type = 'source')
library(TMB)
install.packages("glmmTMB")
library(glmmTMB)
#Fitting a model for all predictors
class(mydata$ParentGlobalID)
mydata <- mydata %>%                                       
  group_by(ParentGlobalID) %>%
  dplyr::mutate(ID = cur_group_id())
mydata
mydata$MouseID<-as.numeric(mydata$ID)
mydata$TimeSinceDeath<-as.numeric(mydata$TimeSinceDeath)
InitialGLM<-glmmTMB(TimeSinceDeath~eyes+infest+fur+scent + (1|MouseID),data=mydata, family = Gamma(link = "log"))
summary(InitialGLM)
mydata$DaysSinceDeath<-ifelse(mydata$TimeSinceDeath>=0&mydata$TimeSinceDeath<=1440,1,0)
BinomialGLM<-glmmTMB(DaysSinceDeath~eyes+infest+fur+scent +(1|MouseID),data=mydata,family=binomial)
summary(BinomialGLM)
summary(InitialGLM)
#Going to test models with various levels of predictors
test1<-glmmTMB(DaysSinceDeath~eyes+infest+fur+scent+(1|MouseID),data=mydata,family=binomial)
test2<-glmmTMB(DaysSinceDeath~eyes+infest+fur+(1|MouseID),data=mydata,family=binomial)
test3<-glmmTMB(DaysSinceDeath~eyes+infest+scent+(1|MouseID),data=mydata,family=binomial)
test4<-glmmTMB(DaysSinceDeath~eyes+fur+scent+(1|MouseID),data=mydata,family=binomial)
test5<-glmmTMB(DaysSinceDeath~infest+fur+scent+(1|MouseID),data=mydata,family=binomial)
test6<-glmmTMB(DaysSinceDeath~eyes+infest+(1|MouseID),data=mydata,family=binomial)
test7<-glmmTMB(DaysSinceDeath~fur+scent+(1|MouseID),data=mydata,family=binomial)
test8<-glmmTMB(DaysSinceDeath~eyes+scent+(1|MouseID),data=mydata,family=binomial)
test9<-glmmTMB(DaysSinceDeath~infest+fur+(1|MouseID),data=mydata,family=binomial)

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
#Test1 is our best model, it includes all the predictors
#Time to make a plot that visualizes these 
install.packages("ggeffects")
library(ggeffects)
marginal1<-ggpredict(BinomialGLM, terms = c("infest","fur","scent","eyes"))
plot(marginal1)
colnames(marginal1)<-c('infest', 'pred', 'se', 'cl', 'cu', 'fur', 'scent', 'eyes')
SecondFigure<-ggplot(data=marginal1, aes(x=as.factor(infest), y=pred, ymin=cl, ymax=cu,color=as.factor(eyes)))+geom_pointrange()+facet_grid(fur~scent)
SecondFigure
SecondFigure2<-SecondFigure+labs(title="Probability of freshness based on carcass")
furnames<-c('0'="Intact",'1'="Removable")
scentnames<-c('0'="No scent",'1'="scent present") 
SecondFigure3<-SecondFigure2+facet_grid(fur~scent,labeller=labeller(fur=as_labeller(furnames),scent=as_labeller(scentnames)))
SecondFigure4<-SecondFigure3+scale_color_discrete(name = "Eyes", labels = c("1" = "Damaged/Absent", "0" = "Round and moist"))
SecondFigure5<-SecondFigure4+labs(x = "Infestation", y = "Probability of freshness")
SecondFigure5

#Ambitious project: merge the 2021 and 2022 datasets
#start by running version controlled mouse analysis.R from last year
##
##BEGIN version controlled mouse analysis.R
##
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
##
##
## END version controlled mouse analysis.R
initial_data$MouseID<-initial_data$ID+100
initial_data$infest<-InfestStatus
initial_data$fur<-furtime
initial_data$scent<-simplescent
initial_data$eyes<-Eyestatus
mixinitial_data<-initial_data[c("MouseID","scent","fur","infest","eyes","Timesincedeath")]
mixinitial_data
mixmydata<-mydata[c("MouseID","scent","fur","infest","eyes","TimeSinceDeath")]
mixmydata
mixmydata<-mixmydata %>% rename("Timesincedeath"="TimeSinceDeath")
mixmydata
mixinitial_data
mixeddata<-rbind(mixmydata,mixinitial_data)
mixeddata

mixedGLM<-glmmTMB(Timesincedeath~fur+scent+infest+eyes+(1|MouseID),data=mixeddata,family=Gamma(link ="log"))
summary(mixedGLM)
mixedsimulationoutput<-simulateResiduals(fittedModel=mixedGLM,plot=TRUE)
#Going to test models with various levels of predictors
mtest1<-glmmTMB(Timesincedeath~eyes+infest+fur+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest2<-glmmTMB(Timesincedeath~eyes+infest+fur+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest3<-glmmTMB(Timesincedeath~eyes+infest+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest4<-glmmTMB(Timesincedeath~eyes+fur+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest5<-glmmTMB(Timesincedeath~infest+fur+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest6<-glmmTMB(Timesincedeath~eyes+infest+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest7<-glmmTMB(Timesincedeath~fur+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest8<-glmmTMB(Timesincedeath~eyes+scent+(1|MouseID),data=mixeddata,family=Gamma(link="log"))
mtest9<-glmmTMB(Timesincedeath~infest+fur+(1|MouseID),data=mixeddata,family=Gamma(link="log"))

simulation1output<-simulateResiduals(fittedModel=mtest1,plot=TRUE)
simulation2output<-simulateResiduals(fittedModel=mtest2,plot=TRUE)
simulation3output<-simulateResiduals(fittedModel=mtest3,plot=TRUE)
simulation4output<-simulateResiduals(fittedModel=mtest4,plot=TRUE)
simulation5output<-simulateResiduals(fittedModel=mtest5,plot=TRUE)
simulation6output<-simulateResiduals(fittedModel=mtest6,plot=TRUE)
simulation7output<-simulateResiduals(fittedModel=mtest7,plot=TRUE)
simulation8output<-simulateResiduals(fittedModel=mtest8,plot=TRUE)
simulation9output<-simulateResiduals(fittedModel=mtest9,plot=TRUE)

mixeddata$DaysSinceDeath<-ifelse(mixeddata$Timesincedeath>=0&mixeddata$Timesincedeath<=1440,1,0)
MixedBinomialGLM<-glmmTMB(DaysSinceDeath~eyes+infest+fur+scent +(1|MouseID),data=mixeddata,family=binomial)
summary(MixedBinomialGLM)
mixedbinomialoutput<-simulateResiduals(fittedModel=MixedBinomialGLM,plot=TRUE)
#plot

marginal1<-ggpredict(MixedBinomialGLM, terms = c("infest","fur","scent","eyes"))
plot(marginal1)
colnames(marginal1)<-c('infest', 'pred', 'se', 'cl', 'cu', 'fur', 'scent', 'eyes')
SecondFigure<-ggplot(data=marginal1, aes(x=as.factor(infest), y=pred, ymin=cl, ymax=cu,color=as.factor(eyes)))+geom_pointrange()+facet_grid(fur~scent)
SecondFigure
SecondFigure2<-SecondFigure+labs(title="Probability of freshness based on carcass")
furnames<-c('0'="Intact",'1'="Removable")
scentnames<-c('0'="No scent",'1'="scent present") 
SecondFigure3<-SecondFigure2+facet_grid(fur~scent,labeller=labeller(fur=as_labeller(furnames),scent=as_labeller(scentnames)))
SecondFigure4<-SecondFigure3+scale_color_discrete(name = "Eyes", labels = c("1" = "Damaged/Absent", "0" = "Round and moist"))
SecondFigure5<-SecondFigure4+labs(x = "Infestation", y = "Time since death (min)")
SecondFigure5