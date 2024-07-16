 

library(dplyr)
library(arules)
library(bnlearn)
library(tidyr)
library(Boruta)
library(knitr)
library(Boruta)
library(randomForest)
library(pROC)
library(gbm)
library(neuralnet)
library(rocc)
library(tree)
library(bnlearn)
library(mice)
library(mi)
library(sem)

jan <- read_csv("CSV Dataset/jan_bayes1.csv")
 
jan<-as.data.frame(jan)
str(jan)
View(jan)

prior<- jan[,c(1:53, 73:112)]

prior < - mice(prior)
prior <-complete(prior)
prior  <-na.omit(prior)

##################################


#We discretize numerical variables
set.seed(123)
table(discretize(jan$Sum_Trauma_Types, method = "cluster", breaks = 3))
table(discretize(jan$CTF2_a, method = "cluster", breaks = 3))
table(discretize(jan$CTF4a, method = "cluster", breaks = 3))
table(discretize(jan$THS10j, method = "cluster", breaks = 3))
table(discretize(jan$CTF5a, method = "cluster", breaks = 3))
table(discretize(jan$THS1a, method = "cluster", breaks = 3))
table(discretize(jan$THS2b, method = "cluster", breaks = 3))
table(discretize(jan$THS3, method = "cluster", breaks = 3))
table(discretize(jan$THS3c, method = "cluster", breaks = 3))
table(discretize(jan$THS4, method = "cluster", breaks = 3))
table(discretize(jan$CESD_TOT, method = "cluster", breaks = 3))
table(discretize(jan$THS9i, method = "cluster", breaks = 3))
table(discretize(jan$TSH9, method = "cluster", breaks = 3))
table(discretize(jan$THS10, method = "cluster", breaks = 3))
table(discretize(jan$THS11, method = "cluster", breaks = 3))
table(discretize(jan$THS11k, method = "cluster", breaks = 3))
table(discretize(jan$THS12, method = "cluster", breaks = 3))
table(discretize(jan$THS12l, method = "cluster", breaks = 3))
table(discretize(jan$THS13, method = "cluster", breaks = 3))
table(discretize(jan$THS13m, method = "cluster", breaks = 3))
table(discretize(jan$THS14n, method = "cluster", breaks = 3))
table(discretize(jan$THS8, method = "cluster", breaks = 3))
table(discretize(jan$THS8h, method = "cluster", breaks = 3))
table(discretize(jan$THS5e, method = "cluster", breaks = 3))
table(discretize(jan$THS4d, method = "cluster", breaks = 3))
table(discretize(jan$CTF10, method = "cluster", breaks = 3))
table(discretize(jan$THS2, method = "cluster", breaks = 3))



#we create new discretized variables

jan <- jan %>%
  mutate(
    PCL_Total_nv = ifelse(PCL_Total < 42, "1", ifelse(PCL_Total < 63.7, "2", "3")),
    CDRS5_nv = ifelse(CDRS5 < 29.4, "1", ifelse(CDRS5 < 68.4, "2", "3")),
    Sum_Trauma_Types_nv = ifelse(Sum_Trauma_Types < 5.72, "1", ifelse(Sum_Trauma_Types < 8.53, "2", "3")),
    CTF2_a_nv = ifelse(CTF2_a < 31.6, "1", ifelse(CTF2_a < 41.7, "2", "3")),
    CTF4a_nv = ifelse(CTF4a < 5.73, "1", ifelse(CTF4a < 11.5, "2", "3")),
    THS10j_nv = ifelse(THS10j < 24.5, "1", ifelse(THS10j < 35.2, "2", "3")),
    CTF5a_nv = ifelse(CTF5a < 12.2, "1", ifelse(CTF5a < 20.3, "2", "3")),
    THS1a_nv = ifelse(THS1a < 13.1, "1", ifelse(THS1a < 27.2, "2", "3")),
    THS2b_nv = ifelse(THS2b < 13.4, "1", ifelse(THS2b < 29.3, "2", "3")),
    THS3_nv = ifelse(THS3 < 5.25, "1", ifelse(THS3 < 54.2, "2", "3")),
    THS3c_nv = ifelse(THS3c < 10.8, "1", ifelse(THS3c < 26.3, "2", "3")),
    THS4_nv = ifelse(THS4 < 6.12, "1", ifelse(THS4 < 38.8, "2", "3")),
    CESD_TOT_nv = ifelse(CESD_TOT < 13.3, "1", ifelse(CESD_TOT < 29.6, "2", "3")),
    TSH8_nv = ifelse(TSH8 < 19.1, "1", ifelse(TSH8 < 31.4, "2", "3")),
    THS8h_nv = ifelse(THS8h < 17.2, "1", ifelse(THS8h < 29.5, "2", "3")),
    THS5e_nv = ifelse(THS5e < 21.4, "1", ifelse(THS5e < 33.2, "2", "3")),
    THS4d_nv = ifelse(THS4d < 10.9, "1", ifelse(THS4d < 34.5, "2", "3")),
    CTF10_nv = ifelse(CTF10 < 0.761, "1", ifelse(CTF10 < 2.7, "2", "3")),
    THS2_nv = ifelse(THS2 < 10.7, "1", ifelse(THS2< 39.2, "2", "3")),
    THS9i_nv = ifelse(THS9i < 26.5, "1", ifelse(THS9i < 34.5, "2", "3")),
    TSH9_nv = ifelse(TSH9 < 29.5, "1", ifelse(TSH9 < 76.2, "2", "3")),
    THS10_nv = ifelse(THS10 < 24.9, "1", ifelse(THS10 < 73.3, "2", "3")),
    THS11_nv = ifelse(THS11 < 24.6, "1", ifelse(THS11 < 72.5, "2", "3")),
    THS11k_nv = ifelse(THS11k < 22.5, "1", ifelse(THS11k < 31.7, "2", "3")),
    THS12_nv = ifelse(THS12 < 19.67, "1", ifelse(THS12 < 67.6, "2", "3")), # Corrected value here
    THS12l_nv = ifelse(THS12l < 18.4, "1", ifelse(THS12l < 31.4, "2", "3")),
    THS13_nv = ifelse(THS13 < 4.5, "1", ifelse(THS13 < 53.7, "2", "3")),
    THS13m_nv = ifelse(THS13m < 18.6, "1", ifelse(THS13m < 37.4, "2", "3")),
    THS14n_nv = ifelse(THS14n < 11.7, "1", ifelse(THS14n < 31.6, "2", "3"))
    
  )

#exclude discretized variables

jan1<- jan %>%
  select (-c(CDRS5,
             Sum_Trauma_Types,
             CTF2_a,
             CTF4a,
             THS10j,
             CTF5a,
             THS1a,
             THS2b,
             THS2,
             THS3,
             THS3c,
             THS4,
             THS4d,
             THS5,
             THS5e,
             TSH8,
             THS8h,
             CESD_TOT,
             THS9i,
             TSH9,
             THS10,
             THS11,
             THS11k,
             THS12,
             THS12l,
             THS13,
             THS13m,
             THS14n,
             PCL_Total  
  ))
str(jan1)

#CHANGE ALL THE VARIABLES TO CATEGORICAL VARIABLE
jan1<-data.frame(lapply(jan1, function(x) if(is.integer(x)) as.numeric(x) else x))
jan1<-data.frame(lapply(jan1, function(x) if(is.numeric(x)) as.factor(x) else x))
jan1<-data.frame(lapply(jan1, function(x) if(is.character(x)) as.factor(x) else x))
str(jan1)
str(em)
####################################################################################################
####################################################################################################
###  
#                     Prior injury Mental Health Condition  
####################################################################################################
###################################################################################################
#########################################################################################
###################    #################################################################
################## 1. PTSD 2. DEPRESSION 3. ANXIETY 4.Adjustment Disorder 5.ADHD ################################
###########################################################################################



############################################################################
# Prior to Military Mental Health Condition  Acute Stress Disorder PTSD: Prior CTF47_1
###########################################################################  
#1.CTF47_1: Prior PTSD
#

Boruta(CTF47_1~.,data=prior1,doTrace=2)->prior.PTSD

print(prior.PTSD,zero.print=".")
plot(prior.PTSD)
plotImpHistory

table_prior <-attStats(prior.PTSD)
print(table_prior)
list_prior<- subset(table_prior,normHits>0.2 )
list_prior
str(list_prior)
mTBI
CDRS7
CDRS9
CDRS10
CDRS12
CDRS13
CDRS15
CDRS18
CDRS20
CDRS23
CDRS2
CDRS3
CTF3
CTF22a
CTF47_2
CTF47_3
THSK
THS2b_nv
THS3c_nv
THS4_nv
THS12_nv
CTF47_1

#
#select variable to build the Bayesian belief network
bnprior_PTSD <-prior1 %>%
  select(mTBI,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS12,
         CDRS13,
         CDRS15,
         CDRS18,
         CDRS20,
         CDRS23,
         CDRS2,
         CDRS3,
         CTF3,
         CTF22a,
         CTF47_2,
         CTF47_3,
         THSK,
         THS2b_nv,
         THS3c_nv,
         THS4_nv,
         THS12_nv,
         CTF47_1)


str(bnprior_PTSD)
set.seed(1234)
boot1 <-boot.strength(bnprior_PTSD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#model:
#  [CDRS9][CDRS10][CDRS12][CDRS18][CDRS23][CDRS3][CTF47_3][THSK][THS3c_nv][THS4_nv][THS12_nv]
#[CDRS7|CDRS18][CDRS13|CDRS9][CDRS15|CDRS3][CDRS20|CDRS23][CDRS2|CDRS3][CTF3|THSK][CTF47_2|CTF47_3]
#[CTF47_1|CTF47_3][mTBI|CDRS7][CTF22a|mTBI][THS2b_nv|mTBI:THS3c_nv]

prior_Dag_PTSD <- model2network("[CDRS9][CDRS10][CDRS12][CDRS18][CDRS23][CDRS3][CTF47_3][THSK][THS3c_nv][THS4_nv][THS12_nv][CDRS7|CDRS18][CDRS13|CDRS9][CDRS15|CDRS3][CDRS20|CDRS23][CDRS2|CDRS3][CTF3|THSK][CTF47_2|CTF47_3][CTF47_1|CTF47_3][mTBI|CDRS7][CTF22a|mTBI][THS2b_nv|mTBI:THS3c_nv]")
plot(prior_Dag_PTSD)


#######################################################################################################

#  Military Mental Health Condition Depression
# CTF47_2: Prior Depression

Boruta(CTF47_2~.,data=prior1,doTrace=2)->prior.Depression

print(prior.Depression,zero.print=".")
plot(prior.Depression)
plotImpHistory

table_prior <-attStats(prior.Depression)
print(table_prior)
list_prior<- subset(table_prior,normHits>0.2 )
list_prior

#list of improtant variable selected using Boruta
mTBI
CDRS9
CDRS10
CDRS16
CDRS17
CDRS19
CDRS21
CDRS22
CDRS24
CDRSTOTAL
CDRS1
CDRS2
CDRS3
CTF22a
CTF47_1
CTF47_3
CTF47_5
THSI
THS14
PCL_Total_nv
Sum_Trauma_Types_nv
CTF2_a_nv
CTF5a_nv
THS11k_nv
CTF47_2


#
#select variable to build the Bayesian belief network
bnprior <-prior1 %>%
  select(mTBI,
         CDRS9,
         CDRS10,
         CDRS16,
         CDRS17,
         CDRS19,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRSTOTAL,
         CDRS1,
         CDRS2,
         CDRS3,
         CTF22a,
         CTF47_1,
         CTF47_3,
         CTF47_5,
         THSI,
         THS14,
         PCL_Total_nv,
         Sum_Trauma_Types_nv,
         CTF2_a_nv,
         CTF5a_nv,
         THS11k_nv,
         CTF47_2
  )

str(bnprior)

boot1 <-boot.strength(bnprior, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
# [mTBI][CDRS9][CDRS10][CDRS16][CDRS19][CDRS21][CDRS24][CDRSTOTAL][CDRS1][CTF47_5][THS14][PCL_Total_nv]
#[Sum_Trauma_Types_nv][CTF2_a_nv][CTF47_2][CDRS17|CDRS16][CDRS3|CDRS16][CTF47_3|CTF47_2]
#[THSI|Sum_Trauma_Types_nv][CTF5a_nv|CTF2_a_nv][THS11k_nv|CTF2_a_nv][CDRS22|CDRS16:CDRS3][CDRS2|CDRS3]
#[CTF22a|mTBI:THSI][CTF47_1|CTF47_3]

prior_Dag_Depression <- model2network("[mTBI][CDRS9][CDRS10][CDRS16][CDRS19][CDRS21][CDRS24][CDRSTOTAL][CDRS1][CTF47_5][THS14][PCL_Total_nv][Sum_Trauma_Types_nv][CTF2_a_nv][CTF47_2][CDRS17|CDRS16][CDRS3|CDRS16][CTF47_3|CTF47_2][THSI|Sum_Trauma_Types_nv][CTF5a_nv|CTF2_a_nv][THS11k_nv|CTF2_a_nv][CDRS22|CDRS16:CDRS3][CDRS2|CDRS3][CTF22a|mTBI:THSI][CTF47_1|CTF47_3]")
plot(prior_Dag_Depression)

#######################################################################################################
#Prior to Military Mental Health Condition Anxiety
# CTF47_3 : Prior Anxiety
set.seed(12345)
Boruta(CTF47_3~.,data=prior1,doTrace=2)->prior.Anxiety

print(prior.Anxiety,zero.print=".")
plot(prior.Anxiety)
plotImpHistory

table_prior <-attStats(prior.Anxiety)
print(table_prior)
list_prior<- subset(table_prior,normHits>0.2 )
list_prior

#list of important variable selected using Boruta
mTBI
CDRS7
CDRS9
CDRS10
CDRS12
CDRS13
CDRS16
CDRS17
CDRS18
CDRS22
CDRSTOTAL
CDRS1
CDRS2
CDRS3
CDRS4
CTF47_1
CTF47_2
PCL_Total_nv
Sum_Trauma_Types_nv
CESD_TOT_nv
THS12_nv
CTF47_3


#
#select variable to build the Bayesian belief network
bnprior_anxiety <-prior1 %>%
  select( mTBI,
          CDRS7,
          CDRS9,
          CDRS10,
          CDRS12,
          CDRS13,
          CDRS16,
          CDRS17,
          CDRS18,
          CDRS22,
          CDRSTOTAL,
          CDRS1,
          CDRS2,
          CDRS3,
          CDRS4,
          CTF47_1,
          CTF47_2,
          PCL_Total_nv,
          Sum_Trauma_Types_nv,
          CESD_TOT_nv,
          THS12_nv,
          CTF47_3
  )

str(bnprior_anxiety)

boot1 <-boot.strength(bnprior_anxiety, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#model:
#model:
# [mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS18][CDRS21][CDRS24][CDRSTOTAL][CDRS1][CDRS3]
#[Sum_Trauma_Types_nv][CESD_TOT_nv][CTF47_3][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3][CDRS22|CDRS3]
#[CDRS2|CDRS3][CTF47_1|CTF47_3][CTF47_2|CTF47_3][THSI|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv]
#[CDRS17|CDRS16][THS12_nv|CTF47_1]

prior_Dag_Anxiety <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS18][CDRS21][CDRS24][CDRSTOTAL][CDRS1][CDRS3][Sum_Trauma_Types_nv][CESD_TOT_nv][CTF47_3][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3][CDRS22|CDRS3][CDRS2|CDRS3][CTF47_1|CTF47_3][CTF47_2|CTF47_3][THSI|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv][CDRS17|CDRS16][THS12_nv|CTF47_1]")
plot(prior_Dag_Anxiety)


#######################################################################################################
#
#Prior to Military Mental Health Condition Adjustment Disorder
#CTF47_4: Prior Adjustment Disorder

Boruta(CTF47_4~.,data=prior1,doTrace=2)->prior.AD

print(prior.AD,zero.print=".")
plot(prior.AD)
plotImpHistory

table_prior <-attStats(prior.AD)
print(table_prior)
list_prior<- subset(table_prior,normHits>0.2 )
list_prior

#list of important variable selected using Boruta
CDRS9
CDRS10
CDRS12
CDRS13
CDRS18
CDRS19
CDRS21
CDRSTOTAL
CDRS1
CDRS2
CDRS4
THS5e_nv



#
#select variable to build the Bayesian belief network
bnprior_AD <-prior1 %>%
  select( CDRS9,
          CDRS10,
          CDRS12,
          CDRS13,
          CDRS18,
          CDRS19,
          CDRS21,
          CDRSTOTAL,
          CDRS1,
          CDRS2,
          CDRS4,
          THS5e_nv,
          CTF47_4
  )

str(bnprior_AD)

boot1 <-boot.strength(bnprior_AD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1
#model:
# [CDRS9][CDRS10][CDRS12][CDRS18][CDRS19][CDRS21][CDRS1][CDRS2][THS5e_nv][CDRS13|CDRS9][CDRSTOTAL|CDRS1]
#[CDRS4|CDRS10][CTF47_4|THS5e_nv]

prior_Dag_AD <- model2network("[CDRS9][CDRS10][CDRS12][CDRS18][CDRS19][CDRS21][CDRS1][CDRS2][THS5e_nv][CDRS13|CDRS9][CDRSTOTAL|CDRS1][CDRS4|CDRS10][CTF47_4|THS5e_nv]")
plot(prior_Dag_AD)

#########################################################################################
#Prior to Military Mental Health Condition: ADHD
# CTF47_5: Prior ADHD
##

Boruta(CTF47_5~.,data=prior1,doTrace=2)->prior.ADHD

print(prior.ADHD,zero.print=".")
plot(prior.ADHD)
plotImpHistory

table_prior <-attStats(prior.ADHD)
print(table_prior)
list_prior<- subset(table_prior,normHits>0.2 )
list_prior


#list of improtant variable selected using Boruta
CDRS6
CDRS7
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS21
CDRS22
CDRS23
CDRS24
CDRS25
CDRS1
CDRS2
CDRS3
CDRS4
THSD
CDRS5_nv
CESD_TOT_nv
TSH8_nv
CTF47_5


#
#select variable to build the Bayesian belief network
bnprior_ADHD <-prior1 %>%
  select(CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRS25,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         THSD,
         CDRS5_nv,
         CESD_TOT_nv,
         TSH8_nv,
         CTF47_5)

str(bnprior_ADHD)
set.seed(1234)
boot1 <-boot.strength(bnprior_ADHD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model_prior:
#CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS3]
#[THSD][CDRS5_nv][TSH8_nv][CTF47_5][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3]
#[CDRS1|CDRS5_nv][CDRS2|CDRS3][CDRS4|CDRS3][CDRS17|CDRS16][CESD_TOT_nv|CDRS1]

prior_Dag_ADHD <- model2network("[CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS3][THSD][CDRS5_nv][TSH8_nv][CTF47_5][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3][CDRS1|CDRS5_nv][CDRS2|CDRS3][CDRS4|CDRS3][CDRS17|CDRS16][CESD_TOT_nv|CDRS1]")
plot(prior_Dag_ADHD)



###############################################################################################
#############################################################################################
##### ########      PRE: Preinjury Mental Health Condition ###################################
#####################################################################################
################### ##############################################################

pre<-jan[,c(1:48,55:59,73:112)]
# imputing missing data
#STEP 0: imputations CDRS
str(pre)
set.seed(123)
pre <- mice(pre)
pre<-complete(pre)
pre <-na.omit(pre)
str(pre)

#we create new discretized variables

pre <- pre %>%
  pre<-data.frame(lapply(pre, function(x) if(is.integer(x)) as.numeric(x) else x))
pre<-data.frame(lapply(pre, function(x) if(is.numeric(x)) as.factor(x) else x))
pre<-data.frame(lapply(pre, function(x) if(is.character(x)) as.factor(x) else x))

pre<- pre %>%
  str(pre)
####################################################################################
# 1 selection of important variables associated to PTSD
# CTF47a_1: Pre PTSD
#####################################################################################

set.seed(1234)

Boruta(CTF47a_1~.,data=pre,doTrace=2)->pre.PTSD

print(pre.PTSD,zero.print=".")
plot(pre.PTSD)


table_pre <-attStats(pre.PTSD)
print(table_pre)
list_pre<- subset(table_pre,normHits>0.2 )
list_pre

#list of important variable selected using Boruta
CDRS6
CDRS7
CDRS13
CDRS15
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS24
CDRS1
CTF10
CTF14a
CTF47a_2
CTF47a_3
CTF47a_4
CTF47a_5
THSH
THSI
THSK
THSM
PCL_Total_nv
Sum_Trauma_Types_nv
CTF2_a_nv
CTF4a_nv
CESD_TOT_nv
CTF10_nv
THS2_nv
THS11_nv


#
#select variable to build the Bayesian belief network
bnpre_PTSD <-pre %>%
  select(CDRS6,
         CDRS7,
         CDRS13,
         CDRS15,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS1,
         CTF10,
         CTF14a,
         CTF47a_2,
         CTF47a_3,
         CTF47a_4,
         CTF47a_5,
         THSH,
         THSI,
         THSK,
         THSM,
         PCL_Total_nv,
         Sum_Trauma_Types_nv,
         CTF2_a_nv,
         CTF4a_nv,
         CESD_TOT_nv,
         CTF10_nv,
         THS2_nv,
         THS11_nv,
         CTF47a_1
  )

str(bnpre_PTSD)

boot1 <-boot.strength(bnpre_PTSD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:

pre_Dag_PTSD <- model2network("[CDRS6][CDRS15][CDRS18][CDRS19][CDRS20][CDRS22][CDRS1][CTF14a][CTF47a_2][CTF47a_5][Sum_Trauma_Types_nv][CTF4a_nv][CTF10_nv][THS2_nv][CDRS7|CDRS18][CDRS13|CDRS22][CDRS21|CDRS22][CDRS24|CDRS22][CTF10|CTF10_nv][CTF47a_4|CTF47a_2][THSH|Sum_Trauma_Types_nv][THSK|Sum_Trauma_Types_nv][THSM|Sum_Trauma_Types_nv][CTF2_a_nv|CTF4a_nv][CESD_TOT_nv|CDRS1][THS11_nv|Sum_Trauma_Types_nv][CTF47a_3|CTF47a_2:CTF47a_4][THSI|THSK][PCL_Total_nv|CESD_TOT_nv][CTF47a_1|CTF47a_3]")
plot(pre_Dag_PTSD)


#######################################################################################################
#Prior to Military Mental Health Condition Depression
#CTF47a_2: Pre Depression
###########################################################################

Boruta(CTF47a_2~.,data=pre,doTrace=2)->pre.Depression

print(pre.Depression,zero.print=".")
plot(pre.Depression)
plotImpHistory

table_pre <-attStats(pre.Depression)
print(table_pre)
list_pre<- subset(table_pre,normHits>0.2 )
list_pre

#list of improtant variable selected using Boruta
CDRS6
CDRS7
CDRS10
CDRS11
CDRS12
CDRS13
CDRS14
CDRS16
CDRS17
CDRS18
CDRS19
CDRS21
CDRS22
CDRS23
CDRS24
CDRS25
CDRSTOTAL
CDRS1
CDRS2
CDRS3
CDRS4
CTF10
CTF14a
CTF47a_1
CTF47a_3
CTF47a_4
THS1
THSL
THS14
PCL_Total_nv
Sum_Trauma_Types_nv
THS3c_nv
THS4_nv
CESD_TOT_nv



#
#select variable to build the Bayesian belief network
bnpre_Depression <-pre %>%
  select( CDRS6,
          CDRS7,
          CDRS10,
          CDRS11,
          CDRS12,
          CDRS13,
          CDRS14,
          CDRS16,
          CDRS17,
          CDRS18,
          CDRS19,
          CDRS21,
          CDRS22,
          CDRS23,
          CDRS24,
          CDRS25,
          CDRSTOTAL,
          CDRS1,
          CDRS2,
          CDRS3,
          CDRS4,
          CTF10,
          CTF14a,
          CTF47a_1,
          CTF47a_3,
          CTF47a_4,
          THS1,
          THSL,
          THS14,
          PCL_Total_nv,
          Sum_Trauma_Types_nv,
          THS3c_nv,
          THS4_nv,
          CESD_TOT_nv,
          CTF47a_2
  )

str(bnpre_Depression)

boot1 <-boot.strength(bnpre_Depression, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#model:

#[CDRS6][CDRS10][CDRS12][CDRS14][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRSTOTAL]
#[CDRS1][CDRS3][CTF10][CTF14a][THS1][THS14][THS3c_nv][THS4_nv][CTF47a_2][CDRS7|CDRS18][CDRS11|CDRS12]
#[CDRS13|CDRS22][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47a_4|CTF47a_2][CESD_TOT_nv|CDRS1]
#[CDRS17|CDRS16][CTF47a_3|CTF47a_4:CTF47a_2][PCL_Total_nv|CESD_TOT_nv][CTF47a_1|CTF47a_3]
#[THSL|PCL_Total_nv][Sum_Trauma_Types_nv|THSL]

pre_Dag_Depression <- model2network("[CDRS6][CDRS10][CDRS12][CDRS14][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRSTOTAL][CDRS1][CDRS3][CTF10][CTF14a][THS1][THS14][THS3c_nv][THS4_nv][CTF47a_2][CDRS7|CDRS18][CDRS11|CDRS12][CDRS13|CDRS22][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47a_4|CTF47a_2][CESD_TOT_nv|CDRS1][CDRS17|CDRS16][CTF47a_3|CTF47a_4:CTF47a_2][PCL_Total_nv|CESD_TOT_nv][CTF47a_1|CTF47a_3][THSL|PCL_Total_nv][Sum_Trauma_Types_nv|THSL]")
plot(pre_Dag_Depression)

#######################################################################################################

# 3. to Military Mental Health: Condition Anxiety
#  CTF47a_3: Pre Anxiety
####

Boruta(CTF47a_3~.,data=pre,doTrace=2)->pre.Anxiety

print(pre.Anxiety,zero.print=".")
plot(pre.Anxiety)
plotImpHistory

table_pre <-attStats(pre.Anxiety)
print(table_pre)
list_pre<- subset(table_pre,normHits>0.2 )
list_pre

#list of important variable selected using Boruta

mTBI
CDRS6
CDRS7
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS24
CDRS25
CDRSTOTAL
CDRS1
CDRS2
CDRS3
CTF10
CTF12
CTF14a
CTF47a_1
CTF47a_2
CTF47a_4
THSE
THSH
THSI
THSJ
THSK
THSL
PCL_Total_nv
Sum_Trauma_Types_nv
THS4_nv
CESD_TOT_nv
TSH8_nv
CTF10_nv
THS12l_nv

#
#select variable to build the Bayesian belief network
bnpre_Anxiety <-pre %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS25,
         CDRSTOTAL,
         CDRS1,
         CDRS2,
         CDRS3,
         CTF10,
         CTF12,
         CTF14a,
         CTF47a_1,
         CTF47a_2,
         CTF47a_4,
         THSE,
         THSH,
         THSI,
         THSJ,
         THSK,
         THSL,
         PCL_Total_nv,
         Sum_Trauma_Types_nv,
         THS4_nv,
         CESD_TOT_nv,
         TSH8_nv,
         CTF10_nv,
         THS12l_nv,
         CTF47a_3
  )

str(bnpre_Anxiety)

boot1 <-boot.strength(bnpre_Anxiety, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1


#model:
#[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS24][CDRS25]
#[CDRSTOTAL][CDRS1][CTF12][CTF14a][CTF47a_2][THSL][THS4_nv][CESD_TOT_nv][TSH8_nv][CTF10_nv][CDRS6|CDRS9]
#[CDRS7|CDRS18][CDRS11|CDRS12][CDRS17|CDRS16][CDRS3|CDRS16][CTF10|CTF10_nv][CTF47a_4|CTF47a_2]
#[PCL_Total_nv|CESD_TOT_nv][Sum_Trauma_Types_nv|THSL][THS12l_nv|THSL][CDRS22|CDRS3][CDRS2|CDRS3]
#[THSE|Sum_Trauma_Types_nv][THSH|mTBI:Sum_Trauma_Types_nv][THSJ|Sum_Trauma_Types_nv]
#[THSK|Sum_Trauma_Types_nv:THS12l_nv][CTF47a_3|CTF47a_2:CTF47a_4][CTF47a_1|CTF47a_3][THSI|THSK]

pre_Dag_Anxiety <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS24][CDRS25][CDRSTOTAL][CDRS1][CTF12][CTF14a][CTF47a_2][THSL][THS4_nv][CESD_TOT_nv][TSH8_nv][CTF10_nv][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS17|CDRS16][CDRS3|CDRS16][CTF10|CTF10_nv][CTF47a_4|CTF47a_2][PCL_Total_nv|CESD_TOT_nv][Sum_Trauma_Types_nv|THSL][THS12l_nv|THSL][CDRS22|CDRS3][CDRS2|CDRS3][THSE|Sum_Trauma_Types_nv][THSH|mTBI:Sum_Trauma_Types_nv][THSJ|Sum_Trauma_Types_nv][THSK|Sum_Trauma_Types_nv:THS12l_nv][CTF47a_3|CTF47a_2:CTF47a_4][CTF47a_1|CTF47a_3][THSI|THSK]")
plot(pre_Dag_Anxiety)


#######################################################################################################
# 4. to Military Mental Health: Condition: Adjustment Disorder
#  CTF47a_4: Adjustment Disorder
####


Boruta(CTF47a_4~.,data=pre,doTrace=2)->pre.AD

print(pre.AD,zero.print=".")
plot(pre.AD)
plotImpHistory
table_pre <-attStats(pre.AD)
print(table_pre)
list_pre<- subset(table_pre,normHits>0.2 )
list_pre

#list of important variable selected using Boruta
mTBI
CDRS6
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS14
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS23
CDRS24
CDRSTOTAL
CDRS1
CDRS3
CDRS4
CTF10
CTF47a_1
CTF47a_2
CTF47a_3
CTF47a_5
THSE
THSI
THSL
THS14
PCL_Total_nv
Sum_Trauma_Types_nv
CESD_TOT_nv
TSH8_nv
THS8h_nv
THS5e_nv
CTF10_nv
THS12l_nv




#
#select variable to build the Bayesian belief network
bnpre_AD <-pre %>%
  select(mTBI,
         CDRS6,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS14,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRSTOTAL,
         CDRS1,
         CDRS3,
         CDRS4,
         CTF10,
         CTF47a_1,
         CTF47a_2,
         CTF47a_3,
         CTF47a_5,
         THSE,
         THSI,
         THSL,
         THS14,
         PCL_Total_nv,
         Sum_Trauma_Types_nv,
         CESD_TOT_nv,
         TSH8_nv,
         THS8h_nv,
         THS5e_nv,
         CTF10_nv,
         THS12l_nv,
         CTF47a_4
  )

str(bnpre_AD)

boot1 <-boot.strength(bnpre_AD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#model:
#model:
#[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS16][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24]
#[CDRSTOTAL][CDRS1][CTF47a_2][CTF47a_5][THSL][THS14][TSH8_nv][THS8h_nv][CTF10_nv][CDRS6|CDRS9]
#[CDRS11|CDRS12][CDRS17|CDRS16][CDRS20|CDRS23][CDRS3|CDRS16][CTF10|CTF10_nv][Sum_Trauma_Types_nv|THSL]
#[CESD_TOT_nv|CDRS1][THS12l_nv|THSL:THS8h_nv][CTF47a_4|CTF47a_2][CDRS4|CDRS3]
#[CTF47a_3|CTF47a_2:CTF47a_4][THSE|Sum_Trauma_Types_nv][THSI|Sum_Trauma_Types_nv]
#[PCL_Total_nv|CESD_TOT_nv][CTF47a_1|CTF47a_3][THS5e_nv|mTBI:THSE]

pre_Dag_AD <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS16][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRSTOTAL][CDRS1][CTF47a_2][CTF47a_5][THSL][THS14][TSH8_nv][THS8h_nv][CTF10_nv][CDRS6|CDRS9][CDRS11|CDRS12][CDRS17|CDRS16][CDRS20|CDRS23][CDRS3|CDRS16][CTF10|CTF10_nv][Sum_Trauma_Types_nv|THSL][CESD_TOT_nv|CDRS1][THS12l_nv|THSL:THS8h_nv][CTF47a_4|CTF47a_2][CDRS4|CDRS3][CTF47a_3|CTF47a_2:CTF47a_4][THSE|Sum_Trauma_Types_nv][THSI|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv][CTF47a_1|CTF47a_3][THS5e_nv|mTBI:THSE]")
plot(pre_Dag_AD)


#######################################################################################
#Pre ADHD

library(Boruta)
#

Boruta(CTF47a_5~.,data=pre,doTrace=2)->pre.ADHD

print(pre.ADHD,zero.print=".")
plot(pre.ADHD)

table_pre <-attStats(pre.ADHD)
print(table_pre)
list_pre<- subset(table_pre,normHits>0.2 )
list_pre

mTBI
CDRS6
CDRS7
CDRS8
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS14
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS23
CDRS24
CDRS25
CDRSTOTAL
CDRS1
CDRS2
CDRS3
CDRS4
CTF47a_2
CTF47a_3
CTF47a_4
PCL_Total_nv
CDRS5_nv
Sum_Trauma_Types_nv
CESD_TOT_nv

# select important variables to build the DAG
pre_ADHD<-pre %>%
  select( mTBI,
          CDRS6,
          CDRS7,
          CDRS8,
          CDRS9,
          CDRS10,
          CDRS11,
          CDRS12,
          CDRS13,
          CDRS14,
          CDRS15,
          CDRS16,
          CDRS17,
          CDRS18,
          CDRS19,
          CDRS20,
          CDRS21,
          CDRS22,
          CDRS23,
          CDRS24,
          CDRS25,
          CDRS1,
          CDRS2,
          CDRS3,
          CDRS4,
          CTF47a_2,
          CTF47a_3,
          CTF47a_4,
          PCL_Total_nv,
          CDRS5_nv,
          Sum_Trauma_Types_nv,
          CESD_TOT_nv,
          CTF47a_5
  )


boot2 <-boot.strength(pre_ADHD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot2  
boot2[(boot2$strength >0.5) & (boot2$direction >= 0.5),]
avg.boot2 <- averaged.network(boot2, threshold =0.85)
avg.boot2

#model:
#  [mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS16][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23]
#[CDRS24][CDRS25][CDRSTOTAL][CDRS1][CTF47a_2][CDRS5_nv][CESD_TOT_nv][CTF47a_5][CDRS6|CDRS9]
#[CDRS7|CDRS18][CDRS8|CDRS14][CDRS11|CDRS12][CDRS17|CDRS16][CDRS20|CDRS23][CDRS3|CDRS16]
#[CTF47a_4|CTF47a_2][PCL_Total_nv|CESD_TOT_nv][Sum_Trauma_Types_nv|mTBI][CDRS2|CDRS3][CDRS4|CDRS3]
#[CTF47a_3|CTF47a_2:CTF47a_4]

pre_Dag_ADHD <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS16][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRSTOTAL][CDRS1][CTF47a_2][CDRS5_nv][CESD_TOT_nv][CTF47a_5][CDRS6|CDRS9][CDRS7|CDRS18][CDRS8|CDRS14][CDRS11|CDRS12][CDRS17|CDRS16][CDRS20|CDRS23][CDRS3|CDRS16][CTF47a_4|CTF47a_2][PCL_Total_nv|CESD_TOT_nv][Sum_Trauma_Types_nv|mTBI][CDRS2|CDRS3][CDRS4|CDRS3][CTF47a_3|CTF47a_2:CTF47a_4]")
plot(pre_Dag_ADHD)


###########################################################################################
################################ POST ##########################################################
################################################################################



post <-jan[,c(1:48,61:66, 73:112)]
str(post)

# imputing missing data
#STEP 0: imputations CDRS
set.seed(123)
post <- mice(post)
str(post)
post<-complete(post)
post <-na.omit(post)

#we create new discretized variables

post <- post %>%
  post<-data.frame(lapply(post, function(x) if(is.integer(x)) as.numeric(x) else x))
post<-data.frame(lapply(post, function(x) if(is.numeric(x)) as.factor(x) else x))
post<-data.frame(lapply(post, function(x) if(is.character(x)) as.factor(x) else x))

post<- post %>%
  select (-c(CDRS5,
             Sum_Trauma_Types,
             CTF2_a,
             CTF4a,
             THS10j,
             CTF5a,
             THS1a,
             THS2b,
             THS2,
             THS3,
             THS3c,
             THS4,
             THS4d,
             THS5,
             THS5e,
             TSH8,
             THS8h,
             CDRSTOTAL,
             CESD_TOT,
             THS9i,
             TSH9,
             THS10,
             THS11,
             THS11k,
             THS12,
             THS12l,
             THS13,
             THS13m,
             THS14n,
             PCL_Total  
  ))

str(post)

#########################################################################

#Prior to Military Mental Health Condition  Acute Stress Disorder PTSD:
set.seed(1234)

Boruta(CTF47b_1~.,data=post,doTrace=2)->post.PTSD

print(post.PTSD,zero.print=".")
plot(post.PTSD)


table_post <-attStats(post.PTSD)
print(table_post)
list_post<- subset(table_post,normHits>0.2 )
list_post

#list of improtant variable selected using Boruta
mTBI
CDRS6
CDRS7
CDRS9
CDRS11
CDRS13
CDRS17
CDRS24
CDRS3
CTF12
CTF15
CTF47b_2
CTF47b_3
CTF47b_4
CTF47b_5
CTF47b_7
THSI
THSK
PCL_Total_nv
Sum_Trauma_Types_nv
CESD_TOT_nv
THS12_nv
THS12l_nv
CTF47b_1

#
#select variable to build the Bayesian belief network
bnpost_PTSD <-post %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS11,
         CDRS13,
         CDRS17,
         CDRS24,
         CDRS3,
         CTF12,
         CTF15,
         CTF47b_2,
         CTF47b_3,
         CTF47b_4,
         CTF47b_5,
         CTF47b_7,
         THSI,
         THSK,
         PCL_Total_nv,
         Sum_Trauma_Types_nv,
         CESD_TOT_nv,
         THS12_nv,
         THS12l_nv,
         CTF47b_1
  )

str(bnpost_PTSD)

boot1 <-boot.strength(bnpost_PTSD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
# [mTBI][CDRS7][CDRS9][CDRS11][CDRS17][CDRS24][CTF12][CTF15][CTF47b_5][CTF47b_7][Sum_Trauma_Types_nv]
#[CESD_TOT_nv][THS12_nv][CTF47b_1][CDRS6|CDRS9][CDRS13|CDRS9][CDRS3|CDRS17][CTF47b_2|CESD_TOT_nv]
#[CTF47b_4|mTBI][THSK|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv][THS12l_nv|Sum_Trauma_Types_nv]
#[CTF47b_3|CTF47b_2][THSI|THSK]

post_Dag_PTSD <- model2network("[mTBI][CDRS7][CDRS9][CDRS11][CDRS17][CDRS24][CTF12][CTF15][CTF47b_5][CTF47b_7][Sum_Trauma_Types_nv][CESD_TOT_nv][THS12_nv][CTF47b_1][CDRS6|CDRS9][CDRS13|CDRS9][CDRS3|CDRS17][CTF47b_2|CESD_TOT_nv][CTF47b_4|mTBI][THSK|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv][THS12l_nv|Sum_Trauma_Types_nv][CTF47b_3|CTF47b_2][THSI|THSK]")
plot(post_Dag_PTSD)


#######################################################################################################
#Post to Military Mental Health Condition Depression
######################################################################################
set.seed(1234)
Boruta(CTF47b_2~.,data=post,doTrace=2)->post.Depression

print(post.Depression,zero.print=".")
plot(post.Depression)
plotImpHistory

table_post <-attStats(post.Depression)
print(table_post)
list_post<- subset(table_post,normHits>0.2 )
list_post

#list of improtant variable selected using Boruta
CDRS6
CDRS7
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS23
CDRS24
CDRS1
CDRS2
CDRS3
CDRS4
CTF11_2
CTF12
CTF15
CTF47b_1
CTF47b_3
CTF47b_4
CTF47b_5
CTF47b_7
THSG
PCL_Total_nv
CDRS5_nv
CESD_TOT_nv
CTF47b_2
#
#select variable to build the Bayesian belief network
bnpost_Depression <-post %>%
  select(CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF11_2,
         CTF12,
         CTF15,
         CTF47b_1,
         CTF47b_3,
         CTF47b_4,
         CTF47b_5,
         CTF47b_7,
         THSG,
         PCL_Total_nv,
         CDRS5_nv,
         CESD_TOT_nv,
         CTF47b_2
  )

str(bnpost_Depression)

boot1 <-boot.strength(bnpost_Depression, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#model:
#  [CDRS9][CDRS10][CDRS12][CDRS13][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS23][CDRS24][CDRS1]
#[CTF12][CTF15][CTF47b_1][CTF47b_4][CTF47b_5][CTF47b_7][THSG][CDRS5_nv][CDRS6|CDRS9][CDRS7|CDRS18]
#[CDRS11|CDRS12][CDRS17|CDRS16][CDRS3|CDRS16][CTF11_2|CTF12][CESD_TOT_nv|CDRS1][CDRS2|CDRS3]
#[CDRS4|CDRS3][PCL_Total_nv|CESD_TOT_nv][CTF47b_2|CESD_TOT_nv][CTF47b_3|CTF47b_2]

post_Dag_Depression <- model2network("[CDRS9][CDRS10][CDRS12][CDRS13][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS23][CDRS24][CDRS1][CTF12][CTF15][CTF47b_1][CTF47b_4][CTF47b_5][CTF47b_7][THSG][CDRS5_nv][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS17|CDRS16][CDRS3|CDRS16][CTF11_2|CTF12][CESD_TOT_nv|CDRS1][CDRS2|CDRS3][CDRS4|CDRS3][PCL_Total_nv|CESD_TOT_nv][CTF47b_2|CESD_TOT_nv][CTF47b_3|CTF47b_2]")
plot(post_Dag_Depression)

#######################################################################################################
#
# 3.Prior to Military Mental Health Condition Anxiety
# CTF47b_3: post Anxiety
#####  

Boruta(CTF47b_3~.,data=post,doTrace=2)->post.Anxiety

print(post.Anxiety,zero.print=".")
plot(post.Anxiety)
plotImpHistory

table_post <-attStats(post.Anxiety)
print(table_post)
list_post<- subset(table_post,normHits>0.2)
list_post

#list of important variable selected using Boruta
mTBI
CDRS6
CDRS9
CDRS10
CDRS12
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS24
CDRS25
CDRS1
CDRS2
CDRS3
CTF47b_1
CTF47b_2
CTF47b_4
CTF47b_5
CTF47b_7
THSL
PCL_Total_nv
CDRS5_nv
THS2b_nv
CESD_TOT_nv
THS12l_nv
THS13_nv
CTF47b_3


#
#select variable to build the Bayesian belief network
bnpost_Anxiety <-post %>%
  select(mTBI,
         CDRS6,
         CDRS9,
         CDRS10,
         CDRS12,
         CDRS13,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS25,
         CDRS1,
         CDRS2,
         CDRS3,
         CTF47b_1,
         CTF47b_2,
         CTF47b_4,
         CTF47b_5,
         CTF47b_7,
         THSL,
         PCL_Total_nv,
         CDRS5_nv,
         THS2b_nv,
         CESD_TOT_nv,
         THS12l_nv,
         THS13_nv,
         CTF47b_3
  )

str(bnpost_Anxiety)

boot1 <-boot.strength(bnpost_Anxiety, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1


#model:
#  [CDRS6][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS25]
#[CDRS1][CTF47b_1][CTF47b_5][CTF47b_7][THSL][CDRS5_nv][THS2b_nv][CESD_TOT_nv][THS13_nv][mTBI|CDRS6]
#[CDRS17|CDRS16][CDRS24|CDRS22][CDRS3|CDRS16][CTF47b_2|CESD_TOT_nv][PCL_Total_nv|CESD_TOT_nv]
#[THS12l_nv|THSL][CDRS2|CDRS3][CTF47b_4|mTBI][CTF47b_3|CTF47b_2]

post_Dag_Anxiety <- model2network("[CDRS6][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS25][CDRS1][CTF47b_1][CTF47b_5][CTF47b_7][THSL][CDRS5_nv][THS2b_nv][CESD_TOT_nv][THS13_nv][mTBI|CDRS6][CDRS17|CDRS16][CDRS24|CDRS22][CDRS3|CDRS16][CTF47b_2|CESD_TOT_nv][PCL_Total_nv|CESD_TOT_nv][THS12l_nv|THSL][CDRS2|CDRS3][CTF47b_4|mTBI][CTF47b_3|CTF47b_2]")
plot(post_Dag_Anxiety)


#######################################################################################################
#4.Post to Military Mental Health Condition: Adjustment Disorder
#  CTF47b_4: Post AD
##

Boruta(CTF47b_4~.,data=post,doTrace=2)->post.AD

print(post.AD,zero.print=".")
plot(post.AD)
plotImpHistory

table_post <-attStats(post.AD)
print(table_post)
list_post<- subset(table_post,normHits>0.2 )
list_post

#list of important variable selected using Boruta
mTBI
CDRS6
CDRS7
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS24
CDRS1
CDRS2
CDRS3
CDRS4
CTF10
CTF14a
CTF22a
CTF47b_1
CTF47b_2
CTF47b_3
CTF47b_5
CTF47b_7
THSI
THSL
PCL_Total_nv
CDRS5_nv
Sum_Trauma_Types_nv
CTF2_a_nv
THS3c_nv
CESD_TOT_nv
THS4d_nv
CTF10_nv
THS12_nv
THS12l_nv
CTF47b_4

#
#select variable to build the Bayesian belief network
bnpost_AD <-post %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF10,
         CTF14a,
         CTF22a,
         CTF47b_1,
         CTF47b_2,
         CTF47b_3,
         CTF47b_5,
         CTF47b_7,
         THSI,
         THSL,
         PCL_Total_nv,
         CDRS5_nv,
         Sum_Trauma_Types_nv,
         CTF2_a_nv,
         THS3c_nv,
         CESD_TOT_nv,
         THS4d_nv,
         CTF10_nv,
         THS12_nv,
         THS12l_nv,
         CTF47b_4
  )

str(bnpost_AD)

boot1 <-boot.strength(bnpost_AD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:

#  [mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS1][CTF14a]
#[CTF47b_1][CTF47b_5][CTF47b_7][THSL][CDRS5_nv][THS3c_nv][THS4d_nv][CTF10_nv][CTF47b_4][CDRS6|CDRS9]
#[CDRS7|CDRS18][CDRS11|CDRS12][CDRS17|CDRS16][CDRS24|CDRS22][CDRS3|CDRS16][CTF10|CTF10_nv][CTF22a|mTBI]
#[Sum_Trauma_Types_nv|THSL:THS4d_nv][CTF2_a_nv|CTF10_nv][CESD_TOT_nv|CDRS1][THS12_nv|THSL:THS4d_nv]
#[THS12l_nv|THSL][CDRS2|CDRS3][CDRS4|CDRS3][CTF47b_2|CESD_TOT_nv][THSI|Sum_Trauma_Types_nv]
#[PCL_Total_nv|CESD_TOT_nv][CTF47b_3|CTF47b_2]


post_Dag_AD <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS16][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS1][CTF14a][CTF47b_1][CTF47b_5][CTF47b_7][THSL][CDRS5_nv][THS3c_nv][THS4d_nv][CTF10_nv][CTF47b_4][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS17|CDRS16][CDRS24|CDRS22][CDRS3|CDRS16][CTF10|CTF10_nv][CTF22a|mTBI][Sum_Trauma_Types_nv|THSL:THS4d_nv][CTF2_a_nv|CTF10_nv][CESD_TOT_nv|CDRS1][THS12_nv|THSL:THS4d_nv][THS12l_nv|THSL][CDRS2|CDRS3][CDRS4|CDRS3][CTF47b_2|CESD_TOT_nv][THSI|Sum_Trauma_Types_nv][PCL_Total_nv|CESD_TOT_nv][CTF47b_3|CTF47b_2]")
plot(post_Dag_AD)

################################################################################################
# 5.Post to Military Mental Health Condition: ADHD
#  CTF47b_5: Post ADHD

library(Boruta)

Boruta(CTF47b_5~.,data=post,doTrace=2)->post.ADHD

print(post.ADHD,zero.print=".")
plot(post.ADHD)


table_post <-attStats(post.ADHD)
print(table_post)
list_post<- subset(table_post,normHits>0.2 )
list_post

bnpost_ADHD<-post %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS14,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRS25,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF12,
         CTF47b_2,
         CTF47b_3,
         CTF47b_4,
         THS14,
         PCL_Total_nv,
         CDRS5_nv,
         CESD_TOT_nv,
         CTF10_nv,
         CTF47b_5
  )




boot3 <-boot.strength(bnpost_ADHD, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot3  
boot3[(boot3$strength >0.5) & (boot3$direction >= 0.5),]
avg.boot3 <- averaged.network(boot3, threshold =0.85)
avg.boot3
#model:
#  [mTBI][CDRS6][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22]
#[CDRS23][CDRS24][CDRS25][CDRS1][CDRS3][CTF12][THS14][CDRS5_nv][CTF10_nv][CTF47b_5][CDRS7|CDRS18]
#[CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47b_4|mTBI][CESD_TOT_nv|CDRS1]
#[CDRS17|CDRS16][CTF47b_2|CESD_TOT_nv][PCL_Total_nv|CESD_TOT_nv][CTF47b_3|CTF47b_2]

post_Dag_ADHD <- model2network("[mTBI][CDRS6][CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS1][CDRS3][CTF12][THS14][CDRS5_nv][CTF10_nv][CTF47b_5][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47b_4|mTBI][CESD_TOT_nv|CDRS1][CDRS17|CDRS16][CTF47b_2|CESD_TOT_nv][PCL_Total_nv|CESD_TOT_nv][CTF47b_3|CTF47b_2]")
plot(post_Dag_ADHD)

#############################################################################################
###########################################################################################
################################################## CURRENT #################################
################ currenttinjury Mental Health Condition  ADHD #################################################
#CTF4c_1:  current PTSD

current<-jan[,c(1:48, 67:71, 73:112)]
# imputing missing data
#STEP 0: imputation s CDRS
set.seed(123)
current <- mice(current)
current
current<-complete(current)
current <-na.omit(current)



current <- current %>%
  current<-data.frame(lapply(current, function(x) if(is.integer(x)) as.numeric(x) else x))
current<-data.frame(lapply(current, function(x) if(is.numeric(x)) as.factor(x) else x))
current<-data.frame(lapply(current, function(x) if(is.character(x)) as.factor(x) else x))

current<- current %>%
  select (-c(CDRS5,
             Sum_Trauma_Types,
             CTF2_a,
             CTF4a,
             THS10j,
             CTF5a,
             THS1a,
             THS2b,
             THS2,
             THS3,
             THS3c,
             THS4,
             THS4d,
             THS5,
             THS5e,
             TSH8,
             THS8h,
             CDRSTOTAL,
             CESD_TOT,
             THS9i,
             TSH9,
             THS10,
             THS11,
             THS11k,
             THS12,
             THS12l,
             THS13,
             THS13m,
             THS14n,
             PCL_Total  
  ))

str(current)

###########################################################################################
#  Military Mental Health Condition  
# CTF4c_1:  Acute Stress Disorder PTSD
##

Boruta(CTF47c_1~.,data=current,doTrace=2)->current.PTSD

print(current.PTSD,zero.print=".")
plot(current.PTSD)
plotImpHistory

table_current <-attStats(current.PTSD)
print(table_current)
list_current<- subset(table_current,normHits>0.2 )
list_current

#list of improtant variable selected using Boruta
mTBI
CDRS6
CDRS7
CDRS9
CDRS10
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS20
CDRS21
CDRS22
CDRS24
CDRS25
CDRS1
CDRS2
CDRS3
CDRS4
CTF10
CTF11_1
CTF11_2
CTF11_4
CTF12
CTF14a
CTF15
CTF22a
CTF47c_2
CTF47c_3
CTF47c_4
THSH
THSI
THSK
THSL
PCL_Total_nv
CDRS5_nv
Sum_Trauma_Types_nv
CTF5a_nv
CESD_TOT_nv
CTF10_nv
TSH9_nv
THS12_nv
THS12l_nv
CTF47c_1
#select variable to build the Bayesian belief network
bncurrent <-current %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS13,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS25,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF10,
         CTF11_1,
         CTF11_2,
         CTF11_4,
         CTF12,
         CTF14a,
         CTF15,
         CTF22a,
         CTF47c_2,
         CTF47c_3,
         CTF47c_4,
         THSH,
         THSI,
         THSK,
         THSL,
         PCL_Total_nv,
         CDRS5_nv,
         Sum_Trauma_Types_nv,
         CTF5a_nv,
         CESD_TOT_nv,
         CTF10_nv,
         TSH9_nv,
         THS12_nv,
         THS12l_nv,
         CTF47c_1
  )

str(bncurrent)

boot1 <-boot.strength(bncurrent, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#  [mTBI][CDRS9][CDRS10][CDRS13][CDRS15][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS24][CDRS25][CDRS1]
#[CDRS3][CTF11_2][CTF11_4][CTF12][CTF14a][CTF15][THSL][CDRS5_nv][CTF5a_nv][CESD_TOT_nv][CTF10_nv]
#[CDRS6|CDRS9][CDRS7|CDRS18][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF10|CTF10_nv]
#[CTF11_1|CTF11_2:CTF11_4][CTF22a|mTBI][CTF47c_2|CESD_TOT_nv][Sum_Trauma_Types_nv|THSL][THS12_nv|THSL]
#[THS12l_nv|THSL][CDRS17|CDRS16][CTF47c_3|CTF47c_2][THSH|Sum_Trauma_Types_nv][THSK|Sum_Trauma_Types_nv]
#[TSH9_nv|THS12_nv][CTF47c_4|CTF47c_2:CTF47c_3][THSI|THSK][CTF47c_1|CTF47c_3]
#[PCL_Total_nv|CESD_TOT_nv:CTF47c_1]

current_Dag_PTSD <- model2network("[mTBI][CDRS9][CDRS10][CDRS13][CDRS15][CDRS18][CDRS19][CDRS20][CDRS21][CDRS22][CDRS24][CDRS25][CDRS1][CDRS3][CTF11_2][CTF11_4][CTF12][CTF14a][CTF15][THSL][CDRS5_nv][CTF5a_nv][CESD_TOT_nv][CTF10_nv][CDRS6|CDRS9][CDRS7|CDRS18][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF10|CTF10_nv][CTF11_1|CTF11_2:CTF11_4][CTF22a|mTBI][CTF47c_2|CESD_TOT_nv][Sum_Trauma_Types_nv|THSL][THS12_nv|THSL][THS12l_nv|THSL][CDRS17|CDRS16][CTF47c_3|CTF47c_2][THSH|Sum_Trauma_Types_nv][THSK|Sum_Trauma_Types_nv][TSH9_nv|THS12_nv][CTF47c_4|CTF47c_2:CTF47c_3][THSI|THSK][CTF47c_1|CTF47c_3][PCL_Total_nv|CESD_TOT_nv:CTF47c_1]")
plot(current_Dag_PTSD)


#######################################################################################################
#Prior to Military Mental Health Condition Depression
# CTF47c_2: Current Depression
set.seed(1234)
Boruta(CTF47c_2~.,data=current,doTrace=2)->current.Depression

print(current.Depression,zero.print=".")
plot(current.Depression)


table_current <-attStats(current.Depression)
print(table_current)
list_current_depression<- subset(table_current,normHits>0.2 )
list_current_depression

#list of improtant variable selected using Boruta
CDRS6
CDRS7
CDRS9
CDRS10
CDRS12
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS21
CDRS22
CDRS23
CDRS24
CDRS25
CDRS1
CDRS2
CDRS3
CDRS4
CTF11_1
CTF11_2
CTF11_4
CTF12
CTF47c_1
CTF47c_3
CTF47c_4
THSG
PCL_Total_nv
CDRS5_nv
CESD_TOT_nv
CTF47c_2


#
#select variable to build the Bayesian belief network
bncurrent_depression <-current%>%
  select( CDRS6,
          CDRS7,
          CDRS9,
          CDRS10,
          CDRS12,
          CDRS13,
          CDRS15,
          CDRS16,
          CDRS17,
          CDRS18,
          CDRS19,
          CDRS21,
          CDRS22,
          CDRS23,
          CDRS24,
          CDRS25,
          CDRS1,
          CDRS2,
          CDRS3,
          CDRS4,
          CTF11_1,
          CTF11_2,
          CTF11_4,
          CTF12,
          CTF47c_1,
          CTF47c_3,
          CTF47c_4,
          THSG,
          PCL_Total_nv,
          CDRS5_nv,
          CESD_TOT_nv,
          CTF47c_2
  )

str(bncurrent_depression)

boot1 <-boot.strength(bncurrent_depression, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#  [CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS1]
#[CDRS3][CTF11_2][CTF11_4][CTF12][THSG][CDRS5_nv][CESD_TOT_nv][CDRS6|CDRS9][CDRS7|CDRS18][CDRS16|CDRS3]
#[CDRS2|CDRS3][CDRS4|CDRS3][CTF11_1|CTF11_2:CTF11_4][CTF47c_2|CESD_TOT_nv][CDRS17|CDRS16]
#[CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_3:CTF47c_2][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]

current_Dag_Depression <- model2network("[CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS1][CDRS3][CTF11_2][CTF11_4][CTF12][THSG][CDRS5_nv][CESD_TOT_nv][CDRS6|CDRS9][CDRS7|CDRS18][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF11_1|CTF11_2:CTF11_4][CTF47c_2|CESD_TOT_nv][CDRS17|CDRS16][CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_3:CTF47c_2][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]")
plot(current_Dag_Depression)

#######################################################################################################

# 3.Prior to Military Mental Health Condition Anxiety
# CTF47c_3: Current Anxiety
#  

Boruta(CTF47c_3~.,data=current,doTrace=2)->current.Anxiety

print(current.Anxiety,zero.print=".")
plot(current.Anxiety)
plotImpHistory

table_current <-attStats(current.Anxiety)
print(table_current)
list_current<- subset(table_current,normHits>0.2)
list_current

#list of important variable selected using Boruta
mTBI
CDRS6
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS21
CDRS22
CDRS24
CDRS1
CDRS2
CDRS3
CDRS4
CTF11_1
CTF11_2
CTF11_4
CTF12
CTF47c_1
CTF47c_2
CTF47c_4
THSH
THSL
THSN
PCL_Total_nv
CDRS5_nv
Sum_Trauma_Types_nv
CESD_TOT_nv
THS5e_nv
TSH9_nv
THS10_nv
THS12l_nv
THS13m_nv
CTF47c_3

#
#select variable to build the Bayesian belief network
bncurrent_Anxiety <-current %>%
  select(mTBI,
         CDRS6,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS21,
         CDRS22,
         CDRS24,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF11_1,
         CTF11_2,
         CTF11_4,
         CTF12,
         CTF47c_1,
         CTF47c_2,
         CTF47c_4,
         THSH,
         THSL,
         THSN,
         PCL_Total_nv,
         CDRS5_nv,
         Sum_Trauma_Types_nv,
         CESD_TOT_nv,
         THS5e_nv,
         TSH9_nv,
         THS10_nv,
         THS12l_nv,
         THS13m_nv,
         CTF47c_3
  )

str(bncurrent_Anxiety)

boot1 <-boot.strength(bncurrent_Anxiety, R=500, algorith ="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#  [mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS24][CDRS1][CDRS3]
#[CTF11_2][CTF11_4][CTF12][THSL][CDRS5_nv][CESD_TOT_nv][THS5e_nv][TSH9_nv][THS13m_nv][CDRS6|CDRS9]
#[CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF11_1|CTF11_2:CTF11_4][CTF47c_2|CESD_TOT_nv]
#[Sum_Trauma_Types_nv|THSL][THS10_nv|TSH9_nv][THS12l_nv|THSL][CDRS17|CDRS16]
#[THSH|mTBI:Sum_Trauma_Types_nv][THSN|Sum_Trauma_Types_nv][CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3]
#[CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]


current_Dag_Anxiety <- model2network("[mTBI][CDRS9][CDRS10][CDRS12][CDRS13][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS24][CDRS1][CDRS3][CTF11_2][CTF11_4][CTF12][THSL][CDRS5_nv][CESD_TOT_nv][THS5e_nv][TSH9_nv][THS13m_nv][CDRS6|CDRS9][CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF11_1|CTF11_2:CTF11_4][CTF47c_2|CESD_TOT_nv][Sum_Trauma_Types_nv|THSL][THS10_nv|TSH9_nv][THS12l_nv|THSL][CDRS17|CDRS16][THSH|mTBI:Sum_Trauma_Types_nv][THSN|Sum_Trauma_Types_nv][CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]")
plot(current_Dag_Anxiety)


#######################################################################################################
#CTF47c_4: Current Adjustment disorder

Boruta(CTF47c_4~.,data=current,doTrace=2)->current.AD

print(current.AD,zero.print=".")
plot(current.AD)
plotImpHistory

table_current <-attStats(current.AD)
print(table_current)
list_current<- subset(table_current,normHits>0.2 )
list_current

#list of important variable selected using Boruta
mTBI
CDRS6
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS16
CDRS19
CDRS20
CDRS21
CDRS22
CDRS23
CDRS24
CDRS1
CDRS2
CDRS4
CTF11_1
CTF12
CTF14a
CTF22a
CTF47c_1
CTF47c_2
CTF47c_3
THSG
PCL_Total_nv
CDRS5_nv
CESD_TOT_nv
THS9i_nv
THS13m_nv
CTF47c_4


#
#select variable to build the Bayesian belief network
bncurrent_AD <-current %>%
  select(mTBI,
         CDRS6,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS16,
         CDRS19,
         CDRS20,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRS1,
         CDRS2,
         CDRS4,
         CTF11_1,
         CTF12,
         CTF14a,
         CTF22a,
         CTF47c_1,
         CTF47c_2,
         CTF47c_3,
         THSG,
         PCL_Total_nv,
         CDRS5_nv,
         CESD_TOT_nv,
         THS9i_nv,
         THS13m_nv,
         CTF47c_4
  )

str(bncurrent_AD)

boot1 <-boot.strength(bncurrent_AD, R=500, algorith="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1

#model:
#  [CDRS9][CDRS10][CDRS12][CDRS13][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS1][CDRS2][CTF12][CTF14a]
#[THSG][CDRS5_nv][CESD_TOT_nv][THS9i_nv][THS13m_nv][CDRS6|CDRS9][CDRS11|CDRS12][CDRS16|CDRS22]
#[CDRS20|CDRS23][CDRS4|CDRS22][CTF11_1|CTF12][CTF47c_2|CESD_TOT_nv][mTBI|CDRS6][CTF47c_3|CTF47c_2]
#[CTF22a|mTBI][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]


current_Dag_AD <- model2network("[CDRS9][CDRS10][CDRS12][CDRS13][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS1][CDRS2][CTF12][CTF14a][THSG][CDRS5_nv][CESD_TOT_nv][THS9i_nv][THS13m_nv][CDRS6|CDRS9][CDRS11|CDRS12][CDRS16|CDRS22][CDRS20|CDRS23][CDRS4|CDRS22][CTF11_1|CTF12][CTF47c_2|CESD_TOT_nv][mTBI|CDRS6][CTF47c_3|CTF47c_2][CTF22a|mTBI][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]")

plot(current_Dag_AD)




#######################################################################################################
#
#CTF47c_5: current ADHD
##

library(Boruta)
#dataprior1
Boruta(CTF47c_5~.,data=current,doTrace=2)->current.ADHD

print(current.ADHD,zero.print=".")
plot(current.ADHD)
plotImpHistory

table_current <-attStats(current.ADHD)
print(table_current)
list_current<- subset(table_current,normHits>0.2 )
list_current

#############
mTBI
CDRS6
CDRS7
CDRS9
CDRS10
CDRS11
CDRS12
CDRS13
CDRS14
CDRS15
CDRS16
CDRS17
CDRS18
CDRS19
CDRS21
CDRS22
CDRS23
CDRS24
CDRS25
CDRS1
CDRS2
CDRS3
CDRS4
CTF47c_1
CTF47c_2
CTF47c_3
CTF47c_4
THSD
THSE
PCL_Total_nv
CDRS5_nv
Sum_Trauma_Types_nv
CESD_TOT_nv
CTF47c_5


#select variable to build the Bayesian belief network
bncurrent_ADHD <-current %>%
  select(mTBI,
         CDRS6,
         CDRS7,
         CDRS9,
         CDRS10,
         CDRS11,
         CDRS12,
         CDRS13,
         CDRS14,
         CDRS15,
         CDRS16,
         CDRS17,
         CDRS18,
         CDRS19,
         CDRS21,
         CDRS22,
         CDRS23,
         CDRS24,
         CDRS25,
         CDRS1,
         CDRS2,
         CDRS3,
         CDRS4,
         CTF47c_1,
         CTF47c_2,
         CTF47c_3,
         CTF47c_4,
         THSD,
         THSE,
         PCL_Total_nv,
         CDRS5_nv,
         Sum_Trauma_Types_nv,
         CESD_TOT_nv,
         CTF47c_5,
  )

str(bncurrent_ADHD)

boot1 <-boot.strength(bncurrent_ADHD, R=500, algorith="hc",
                      algorithm.args = list(score = "bde", iss =10))
boot1  
boot1[(boot1$strength >0.5) & (boot1$direction >= 0.5),]
avg.boot1 <- averaged.network(boot1, threshold =0.85)
avg.boot1


#model:
# [CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25]
#[CDRS1][CDRS3][CDRS5_nv][Sum_Trauma_Types_nv][CESD_TOT_nv][CTF47c_5][CDRS6|CDRS9][CDRS7|CDRS18]
#[CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47c_2|CESD_TOT_nv][THSD|Sum_Trauma_Types_nv]
#[THSE|Sum_Trauma_Types_nv][mTBI|CDRS6][CDRS17|CDRS16][CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3]
#[CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]


current_Dag_ADHD <- model2network("[CDRS9][CDRS10][CDRS12][CDRS13][CDRS14][CDRS15][CDRS18][CDRS19][CDRS21][CDRS22][CDRS23][CDRS24][CDRS25][CDRS1][CDRS3][CDRS5_nv][Sum_Trauma_Types_nv][CESD_TOT_nv][CTF47c_5][CDRS6|CDRS9][CDRS7|CDRS18][CDRS11|CDRS12][CDRS16|CDRS3][CDRS2|CDRS3][CDRS4|CDRS3][CTF47c_2|CESD_TOT_nv][THSD|Sum_Trauma_Types_nv][THSE|Sum_Trauma_Types_nv][mTBI|CDRS6][CDRS17|CDRS16][CTF47c_3|CTF47c_2][CTF47c_1|CTF47c_3][CTF47c_4|CTF47c_2:CTF47c_3][PCL_Total_nv|CTF47c_1:CESD_TOT_nv]")
plot(current_Dag_ADHD )

#######################################################################################

#Find the probability distribution




##########################
# Perform factor analysis

prior_FA <-fa.parral(jan, fm ="minres", fa ="fa")
John Tra, PhD
Compass Government Solutions


