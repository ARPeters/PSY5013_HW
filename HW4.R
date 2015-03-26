library(foreign)
library(knitr)
ds<-read.table("Student_Survey_Data.dat")
colnames(ds)<-c("ID", "Gender", "Age", "Party", "Marijuana")
head(ds)

#ID: case id.
#Gender: males = 1 and females = 2.  
#Age: students' age.
#Party : a categorical variable which contains three political parties: Republican = 1, Democrat = 2, and Independent = 3.  
#Marijuan: attitudes toward legalization of marijuana. High scores on the MARIJUAN variable reflect more positive attitudes toward legalization.

#########################
# @knitre Q1
#Question 1: 
ds$Female<-ifelse(ds$Gender==1, 0, 1)
MarijuanaByGender<-lm(Marijuana~Female, data=ds)
summary(MarijuanaByGender)


#Question 2:
ds$DEM<-ifelse(ds$Party==2, 1, 0)
ds$IND<-ifelse(ds$Party==3, 1, 0)
MarijuanaByParty<-(lm(Marijuana~DEM+IND, data=ds))
summary(MarijuanaByParty)

#Question 3:
ds$DEMUE<-ifelse(ds$Party==2, 1, ifelse(ds$Party==1, 0, -1))
ds$REPUE<-ifelse(ds$Party==1, 1, ifelse(ds$Party==2, 0, -1))
MarijuanaByParty<-(lm(Marijuana~DEMUE+REPUE, data=ds))
summary(MarijuanaByParty)
