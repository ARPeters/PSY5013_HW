library(foreign)
library(psych)
library(influence.ME)
install.packages("lme4")
library(lme4)
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

cbind(ds, hv)

plot(hatvalues(model1, type=""))
plot(model1)



