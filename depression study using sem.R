#Depression in the Marine officiers  after 6 months of training
library(psych)
library(GPArotation)
library(readxl)
library(lavaan)
library(DataExplorer)
library(arules)
library(caret)
library(ROCR)
library(Hmisc)
library(bnlearn)
library(CHAID)
library(tableone)
library(missForest)
library(dplyr)
library(mice)
#Data 

PrePost <- read_excel("C:/Users/jtra0/Downloads/PrePost_10062021.xlsx")
View(PrePost)

set.seed(1364)
head(PrePost)
#baseline emoitions score
em<-subset(PrePost,select =c(
  depressionscore,
  gad7score,                                                              
  perceivedstresscore,                                                    
  painscore,                                                             
  burnoutscore,                 
  Epworthsleepscore,                                                      
  insomniaseverscore,
  SEX,
  AnyInjury,
  RACE_FINAL,
  COMM_SOURCE)) 

str(em)
#create 2 different dataset 

#fit on different data set of graduation
em_b<-em[c(1:1090),]# baseline
em_g <-em[c(1091:2180),]#post graduation
#create tableone
#create demographic subset
em_gdemog <-subset(em_g,select =c(SEX,AnyInjury,RACE_FINAL,COMM_SOURCE))
em_gdemog[] <- lapply(em_gdemog, function(x) {
  if(is.numeric(x)) as.factor(as.character(x)) else x
})
sapply(em_gdemog, class)

#psycho-social

em_gpsych <-subset(em_g,select =c(depressionscore,
                                  gad7score,                                                            
                                  painscore,                                                             
                                  burnoutscore,                 
                                  Epworthsleepscore,
                                  insomniaseverscore
                              ))     

#CHANGE ALL THE VARIABLES TO NUMERICAL VARIABLEs

em_gpsych[] <- lapply(em_gpsych, function(x) {
  if(is.character(x)) as.numeric(as.numeric(x)) else x
})
sapply(em_gpsych, class)

em_g<-cbind(em_gdemog,em_gpsych)
head(em_g)
#Create table one
vars <- c("depressionscore",
  "gad7score",                                                              
  "painscore",                                                             
  "burnoutscore",                 
  "Epworthsleepscore",                                                      
  "insomniaseverscore",
  "SEX",
  "AnyInjury",
  "RACE_FINAL",
  "COMM_SOURCE")
 
## Create Table 1 stratified by gad7score 
tableOne <- CreateTableOne(vars = vars, strata = c("SEX"), data = em_g)
tableOne
tableOne <-as.data.frame(tableOne)
 


parallel_FA  <-fa.parallel(em_g, fm = "minres", fa = "fa")
# build  SEM model
 model <-'
#modelspecifications
#measurement
gad7score~ depressionscore +insomniaseverscore+painscore+burnoutscore

 depressionscore~insomniaseverscore+painscore
 depressionscore ~Epworthsleepscore
 depressionscore ~burnoutscore
 
 insomniaseverscore~painscore
 insomniaseverscore~Epworthsleepscore
 insomniaseverscore~burnoutscore
 
 
 insomniaseverscore~~insomniaseverscore
 depressionscore~~depressionscore
 gad7score~~gad7score 
'
#SEM
fit1 <- sem(model, data=em_g)
summary(fit1,fit.measures=TRUE)
fitMeasures(fit1,c("cfi", "rmsea","srmr"))


#compared to the same model at baseline
fit2 <- sem(model, data=em_b)
summary(fit2,fit.measures=TRUE)
fitMeasures(fit2,c("cfi", "rmsea","srmr"))

anova (fit1,fit2)

#LAVAAN
fit3 <- lavaan(model, data=em_g)
summary(fit3,fit.measures=TRUE)
fitMeasures(fit3,c("cfi", "rmsea","srmr"))

library(semPlot)
semPaths(fit1, what ="paths", whatLabels = "Stand", rotation = 1)