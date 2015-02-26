rm(list = ls(all.names = TRUE))
library(foreign)

#Reading in Data
ds<-read.table("health.dat")
colnames(ds)<-c("case", "age", "income", "havephys", "physrec", "womrel", "lump", "suscept1", "severe1", "benefit1", "barrier1", "intent1", "comply")

#Question1
#Reformatting table into that form from Slideshow 11, slide 14
Model1<-lm(intent1~havephys+physrec,data=ds)
Model1
Model2<-lm(intent1~havephys+physrec+lump+womrel, data=ds)
Model2
Model3<-lm(intent1~havephys+physrec+lump+womrel+suscept1+severe1+benefit1+barrier1, data=ds)
Model3

Model1Coef    <-round(summary(Model1)$coefficients[,1], digits=3)
Model1Coef    <-c(Model1Coef, ".",".",".",".",".",".")
Model1Se      <-round(summary(Model1)$coefficients[,2], digits=3)
Model1Se      <-c(Model1Se, ".",".",".",".",".",".")

Model1subtable<-cbind(Model1Coef, Model1Se)

Model2Coef    <-round(summary(Model2)$coefficients[,1], digits=3)
Model2Coef    <-c(Model2Coef, ".",".",".",".")
Model2Se      <-round(summary(Model2)$coefficients[,2], digits=3)
Model2Se      <-c(Model2Se, ".",".",".",".")

Model2subtable<-cbind(Model2Coef, Model2Se)

Model3Coef    <-round(summary(Model3)$coefficients[,1], digits=3)
Model3Se      <-round(summary(Model3)$coefficients[,2], digits=3)
Model3subtable<-cbind(Model3Coef, Model3Se)

hrtable<-cbind(Model1subtable, Model2subtable, Model3subtable)
hrtable<-data.frame(hrtable)
hrtable1<-hrtable[2:9,]
hrtable1<-data.frame(hrtable1, row.names=c("havephys", "physrec", "lump", "womrel", "suscept1", "severe1", "benefit1", "barrier1"))

yhavephys <-cor(x=ds$havephys, y=ds$intent1)
yphysrec  <-cor(x=ds$physrec, y=ds$intent1)
ylump     <-cor(x=ds$lump, y=ds$intent1)
ywomrel   <-cor(x=ds$womrel, y=ds$intent1)
ysuscept  <-cor(x=ds$suscept1, y=ds$intent1)
ysevere   <-cor(x=ds$severe1, y=ds$intent1)
ybenefit  <-cor(x=ds$benefit1, y=ds$intent1)
ybarrier  <-cor(x=ds$barrier, y=ds$intent1)

cor.with.y<-round(as.vector(c(yhavephys, yphysrec, ylump, ywomrel, ysuscept, ysevere, ybenefit, ybarrier)), digits=3)

Q1table<-cbind(cor.with.y, hrtable1)
Q1table

#Question 2
summary(Model1)

anova(Model1, Model2, Model3)

#Question 3
dsGPA<-read.table("HW3_GPA.dat")
colnames(dsGPA)<-c("CGPA", "HGPA", "SAT", "SEX")

#Question 3-1

#A
#Semipartial correlation between CGPA and SAT by partialling effect of HGPA from CGPA.
#Regress CGPA onto HGPA; compute residuals; find correlation between SAT and residuals of CGPA
CregH<-lm(CGPA~HGPA, data=dsGPA)
CregHresiduals<-as.vector(CregH$residuals)
dsGPA<-cbind(dsGPA, CregHresiduals)

SATcorCresid<-cor(dsGPA$CregHresiduals, dsGPA$SAT)
SATcorCresid

#B
#Semipartial of CGPA and SAT, controlling for HGPA, using correlation coefficients
ScorC<-cor(dsGPA$SAT, dsGPA$CGPA)
ScorH<-cor(dsGPA$SAT, dsGPA$HGPA)
CcorH<-cor(dsGPA$CGPA, dsGPA$HGPA)

ScorC
ScorH
CcorH

#C
#Semipartial of CGPA and SAT, removing effects of HGPA on CGPA, using squared multiple correlation coefficients.
ModelSATonCH<-lm(SAT~CGPA+HGPA, data=dsGPA)
summary(ModelSATonCH)

ModelSATonH<-lm(SAT~HGPA, data=dsGPA)
summary(ModelSATonH)

#Question 3-2

#A
#Computing the correlation of the residuals of SAT regressed onto HGPA and the residuals of CGPA regressed onto HGPA.
SregH<-lm(SAT~HGPA, data=dsGPA)
SregHresiduals<-as.vector(SregH$residuals)
dsGPA<-cbind(dsGPA, SregHresiduals)

SresidCorCresid<-cor(dsGPA$SregHresiduals, dsGPA$CregHresiduals)
SresdiCorCresid

#B
#Semipartial of CGPA and SAT, controlling for HGPA, using correlation coefficients
ScorC<-cor(dsGPA$SAT, dsGPA$CGPA)
ScorH<-cor(dsGPA$SAT, dsGPA$HGPA)
CcorH<-cor(dsGPA$CGPA, dsGPA$HGPA)

ScorC
ScorH
CcorH

#C
#Semipartial of CGPA and SAT, removing effects of HGPA on CGPA, using squared multiple correlation coefficients.
ModelSATonCH<-lm(SAT~CGPA+HGPA, data=dsGPA)
summary(ModelSATonCH)

ModelSATonH<-lm(SAT~HGPA, data=dsGPA)
summary(ModelSATonH)


