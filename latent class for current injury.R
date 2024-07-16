#Latent variable analysis

getwd()
setwd("~/datasets/impact2/")
library("readxl")
library(psych)
library(MASS)
library(caret)
library(readr)
library(questionr)
library(tableone)
library(designmatch)
library(lavaan)
library(psych)
# xls files
my_data <- read_csv("impactcurrentinjury2.csv")
# xlsx files
str(my_data)
describe(my_data)
summary(my_data)
my_data1 <-my_data
#CHANGE ALL THE VARIABLES TO CATEGORICAL VARIABLE
#my_dataCat <- my_data1[,c(1:38,45)]
#my_dataNum <- my_data1[,c(39:44)]
#my_dataCat[] <- lapply(my_dataCat, function(x) {
 # if(is.numeric(x)) as.factor(as.character(x)) else x
#})
#sapply(my_dataCat, class)

#str(my_data1$Sex)

#my_data1 <- cbind(my_dataCat,my_dataNum)
#str(my_data1)
#library(DataExplorer)
#create_report(my_data1)
#describe(my_data1)
 
 #correlation of pain per gender
my_data$Sex<-as.factor(my_data$Sex)
# is there any correlation between current pain and anklesprain?
cor(my_data$currentpain,my_data$anklesprain)
  # [1] 0.1347384 non correlation too low
cor(my_data$currentpain,my_data$Sex)
 #[1] -0.1057604 
cor(my_data$currentpain,my_data$Total_days)
#[1] -0.1259462
attach(my_data)
str(my_data)
#currentpain 
fit1 <-glm(currentpain~anklesprain+upperextrempain24+lowerextrempain24+headnecktrunkpain24,
           family=binomial, data =my_data)
summary(fit1)

fit2 <-glm(currentpain~upperextrempain24+Total_days+Sex,family=binomial, data =my_data)
summary(fit2)
fit3 <-glm(currentpain~headneckpain24+rtshouldpain24+leftshouldpain24+rtupperarmpain24
           +leftupperarmpain24 +rtelbowpain24+ leftelbowpain24+ chestpain24+ abdomenpain24
           +upperbackpain24+ lowerbackpain24+ rthippain24 +lefthippain24 ,family=binomial, data =my_data[,-c(1:2)])
summary(fit3)

fit4<- glm(currentpain~rtkneepain24+leftkneepain24+rtlegpain24+leftlegpain24+rtanklepain24+
             leftanklepain24,family=binomial, data =my_data)
summary(fit4)

fit5<-glm(currentpain~painactivity24r+painsleep24r+painmood24r+painstress24r+paininterferencescore24
          +Total_days+Total_days+Sex,family=binomial, data =my_data)
summary(fit5)

#convert data into categorical varibales and analyse again

#CHANGE ALL THE VARIABLES TO CATEGORICAL VARIABLE
my_dataCat <- my_data1[,c(1:40,41)]
my_dataNum <- my_data1[,41]
my_dataCat[] <- lapply(my_dataCat, function(x) {
 if(is.numeric(x)) as.factor(as.character(x)) else x
})
sapply(my_dataCat, class)

str(my_data1$Sex)

my_data1 <- cbind(my_dataCat,my_dataNum)
str(my_data1)

create_report(my_data1)
describe(my_data1)

latup <-cbind(headneckpain24,rtshouldpain24,leftshouldpain24,rtupperarmpain24,leftupperarmpain24)~currentpain

m1<-poLCA(latup,my_data1,nclass=4,nrep=5,maxiter=8000)
summary(m1)
pm <- cbind(1,c(1:3))

exb <- exp(pm %*% m1$coeff) 
matplot(c(1:3),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
        main="",
        xlab="",
        ylab="",lwd=2,col=1)

latgrp<-cbind(lowerextrempain24,painrating24R,painactivity24r,painsleep24r,painstress24r,paininterferencescore24)~currentpain
m2<-poLCA(latgrp,my_data1,nclass=4,nrep=5,maxiter=8000)

