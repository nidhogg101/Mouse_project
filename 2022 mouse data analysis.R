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