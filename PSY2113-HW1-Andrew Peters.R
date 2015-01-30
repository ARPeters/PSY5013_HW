library(foreign)
ds<-read.csv("gpa.csv")
SAT<-ds$SAT
GPA<-ds$GPA
Prevach<-ds$Prevach

dsSATGPA<-ds[,1:2]
head(dsSATGPA)

#1. Make a scatterplot of GPA against SAT. What can you say about their association in terms of strength, direction, and form?

plot(GPA, SAT)

#There appears to be a loose, positive, linear correlation with a great deal of variance. 



#2. Calculate the Pearson correlation coefficient between GPA and SAT and report its significance. Interpret this correlation 
#   coefficient and state why causation cannot be concluded here.
q<-cor.test(GPA, SAT)
as.numeric(q$estimate)

GPArel*SATrel

as.numeric(q$estimate)/(sqrt(GPArel*SATrel))
#   Correlation coefficient is 3.762257, p-value=7.94e -10, 95% Conf.int: [0.2645, 0.4779818]
#   This suggests positive correlation between the two. 
#   Correlation does not equal causation. 

#3. If the reliability coefficients are known to be .9 and .85 for GPA and SAT, respectively, then take an appropriate statistical
#   procedure to correct for the attenuation in the correlation coefficient. What does the disattenuated correlation mean? 

#   Attenuated correlation is a correlation that attempts to account for error due to imperfect reliability in the variables.
#   correction formula = cor(x*y)/sqrt(reliabilityX*reliabilityY)
GPArel<-0.9
SATrel<-0.85

source("Utilities/rDisattenuated.r")
rDisattenuated(ds$GPA, ds$SAT, GPArel, SATrel)

#  The disattenuated correlation is slightly higher, suggesting that the true r value between the latent variables is stronger than 
#  is the correlation between the two observed sets of data. Supposedly.  

#4.Report the descriptive statistics (including mean, SD, range, skewness, and kurtosis) for both GPA and SAT. What can you say 
# about the distributions of GPA and SAT?

mean(ds$GPA)
sd(ds$GPA)
range(ds$GPA)

mean(ds$SAT)
sd(ds$SAT)
range(ds$SAT)

