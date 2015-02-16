library(foreign)
library(psych)
library(influence.ME)
library(lme4)
library(car)
library(qqnorm)
library(het.test)
library(nortest)
library(het.test)

install.packages("QuantPsyc")
ds<-read.csv("gpa.csv")
head(ds)
colnames(ds)
summary(ds$Prevach)

Highach<-ifelse(ds$Prevach<50.53, 0, 1)

ds<-cbind(ds, Highach)
test<-t.test(ds$GPA~Highach)

str(test)

dsHighach<-ds[ds$Highach==1,]
dsLowach<-ds[ds$Highach==0,]

mean(dsHighach$GPA)
sampleVarHigh<-var(dsHighach$GPA)



mean(dsLowach$GPA)
sampleVarHigh<-var(dsHighach$GPA)


sampleStdvHigh<-sqrt(sampleVarHigh)
length(dsHighach$GPA)

attach(ds)
detach(ds)
plot(GPA, SAT)
detach(ds)


#Leverage
model1<-lm(GPA~SAT, data=ds)
hatvalues(model1)

hv<-as.data.frame(hatvalues(model1))
mn<-mean(hatvalues(model1))

hv$warn<-ifelse(hv[,'hatvalues(model1)']>3*mn, "x3", 
                ifelse(hv[,'hatvalues(model1)']>2*mn, "x2", 0))
hv

dsNew<-cbind(ds, hv)

#Individuals with high leverage:
dsHighL<-dsNew[dsNew$warn!=0,]

#Leverage for reversed model
#model2<-lm(SAT~GPA, data=ds)
#hatvalues(model2)

#hv2<-as.data.frame(hatvalues(model2))
#mn2<-mean(hatvalues(model2))

#hv2$warn<-ifelse(hv2[,'hatvalues(model2)']>3*mn2, "x3", 
#                ifelse(hv2[,'hatvalues(model2)']>2*mn2, "x2", 0))

#dsNew2<-cbind(ds, hv2)

#Individuals with high leverage:
#dsHighL2<-dsNew2[dsNew2$warn!=0,]


#Individuals with high cook's d
#Note: High Cook's D = 4/(N-K-1) = (4/(250-1-1))=4/248 = 0.01612903

influenceObject<-(model1)
cooks.distance(influenceObject)

cooksD<-as.data.frame(cooks.distance(influenceObject))
colnames(cooksD)<-c("Distance")
cooksD$warnD<-ifelse(cooksD$Distance>(4/248), "High", 0)

dsNew<-cbind(dsNew, cooksD)
dsCooksDHigh<-dsNew[dsNew$warnD!=0,]

#Studentized residuals
rstudent(model1)
rStudentized<-as.data.frame(rstudent(model1))
colnames(rStudentized)<-c("warnRstud")
dsNew<-cbind(dsNew, rStudentized)
dsNew$warnRstud<-ifelse(rStudentized>=1.96, "High", ifelse(rStudentized<=-1.96, "Low", 0))
dsNew$warnRstud<-as.data.frame(dsNew$warnRstud)
dsRstudWarn<-dsNew[dsNew$warnRstud!=0,]

colnames(dsNew)<-c("SAT", "GPA", "Prevach", "Highach", "GPAHat", "warnLeverage", "Distance", "warnD", "StudRes", "warnRstud")
colnames(dsRstudWarn)<-c("SAT", "GPA", "Prevach", "Highach", "GPAHat", "warnLeverage", "Distance", "warnD", "StudRes", "warnRstud")

dsRstudWarn

dsFinal<-dsNew[dsNew$warnLeverage!=0 | dsNew$warnD!=0 | dsNew$warnRstud!=0,]
dsFinal
colnames(dsFinal)
dsFinal2<-dsFinal[,c(1,2,3,6,8,10)]
colnames(dsFinal2)

#With "outliers" removed.
dsNeutered<-dsNew[dsNew$warnD==0 | dsNew$warnRstud==0, ]

model1<-lm(GPA~SAT, data=ds)
modelNeutered<-lm(dsNeutered$GPA~dsNeutered$SAT)

summary(model1)
summary(modelNeutered)

#Normality of errors
StudentizedResiduals<-rstudent(model1)
hist(StudentizedResiduals)
str(shapiro.test(GPAonSAT$residuals))

#Homoskedasticity of erros
#White's test
GPAonSAT<-model1
spreadLevelPlot(GPAonSAT)

GPAonSATnew<-lm(GPA~SAT+(SAT*SAT), data=ds)

whites.htest(model1)
#Independence of one residual on preceding observation
durbinWatsonTest(GPAonSAT)

######################################################
#Question 3
######################################################

colnames(ds)
mean(ds$SAT)
sd(ds$SAT)
var(ds$SAT)

mean(ds$GPA)
sd(ds$GPA)
var(ds$GPA)

mean(ds$Prevach)
sd(ds$Prevach)
var(ds$Prevach)

cor(ds$GPA, ds$SAT)
cor(ds$GPA, ds$Prevach)
cor(ds$SAT, ds$Prevach)

.328*.503
X<-(0.543/10.572)
Y<-((0.328-(0.376*0.503))/(1-(0.503*0.503)))
X*Y

model2<-lm(GPA~SAT+Prevach, data=ds)
2.7-(.001734*504.6)-(.00955*50.529)

lm.beta(model2)
library(QuantPsyc)

head(ds)
SATW<-vector()
for(i in 1:length(ds$GPA)){
  SATW[i]<-((ds$SAT[i]-mean(ds$SAT))/sd(ds$SAT))  
}

PVCW<-vector()
for(i in 1:length(ds$GPA)){
  PVCW[i]<-((ds$Prevach[i]-mean(ds$Prevach))/sd(ds$Prevach))  
}


GPAW<-vector()
for(i in 1:length(ds$GPA)){
  GPAW[i]<-((ds$GPA[i]-mean(ds$GPA))/sd(ds$GPA))  
}

#I'm an idiot.
model3<-lm(GPA~SAT+Prevach, data=dsW)
model4<-lm(GPAW~SATW+PVCW, data=dsW)

summary(model3)

summary(model4)
