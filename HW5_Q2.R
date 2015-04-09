rm(list = ls(all.names = TRUE))
library(foreign)
library(mi)

ds<-read.csv("hsb_mar.csv")
colnames(ds)

Missingness<-c(sum(is.na(ds$FEMALE)), sum(is.na(ds$PROG)), sum(is.na(ds$READ)), 
                   sum(is.na(ds$WRITE)), sum(is.na(ds$MATH)), sum(is.na(ds$SCIENCE)))

Variables<-c("FEMALE", "PROG", "READ", "WRITE", "MATH", "SCIENCE")

MissingnessProportion<-Missingness/200

dsMissingnessTable<-cbind(Variables, Missingness, MissingnessProportion)
dsMissingnessTable

#####Question 2

#A

dsComplete<-na.omit(ds)
ModelComplete<-glm(SOCST~WRITE+READ+MATH+FEMALE, data=dsComplete)
summary(ModelComplete)

#B

dsInfo<-mi.info(ds)
dsMI<-mi(object=ds, info=dsInfo, n.imp=5)
fit<-lm.mi(SOCST~WRITE+READ+MATH+FEMALE, dsMI)
fit@mi.pooled
