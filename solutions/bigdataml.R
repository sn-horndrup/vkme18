setwd("~/Documents/GitHub/vkme18")

pacman::p_load(tidyverse,
               rpart, #regression trees
               rpart.plot, #plot p�nere regressionstr�er
               glmnet) #lasso

#big-ish data s�t: samlede eurobarometer 1973-2002
eb<-readRDS("data/14_eurobarometer.rds") 

#skal f�rst lave nyt data uden NA's
eb2<-eb %>% 
  select(membrshp,age,sex,educ,income,lrs,matpmat) %>% 
  na.omit() %>% 
  mutate(membrshp01=ifelse(membrshp>1,0,1)) #bytter rundt s� 1=pro-EU

#baseline: en logit-model
ebformula<-as.formula(membrshp01~age+sex+educ+income+lrs+matpmat)
summary(logit<-glm(ebformula,data=eb2,family="binomial"))

#fit et regressionstr�
regtree<-rpart(ebformula,data=eb2,control=rpart.control(minsplit=30, cp=0.0001))
prp(regtree)

#til LASSO skal vi bruge en matrice kun med inputvariablene
ebinputmat<-as.matrix(dplyr::select(eb2,-membrshp,-membrshp01))

#fit en LASSO-regression
lasso<-glmnet(x=ebinputmat,y=eb2$membrshp01,family="binomial")

#plot lasso koefficienter som funktion af tuningparameteren lambda
plot(lasso,xvar="lambda",label=T)
lasso$beta@Dimnames[[1]] #navne der svarer til labels p� plottet

#prediktion: lad os pr�ve at forudsige en holdningen for en typisk VKM-studerende
#dvs. ung, h�j udd, semi-venstreorienteret
vkmdat<-as.matrix(data.frame(age=25,sex=1,educ=10,income=4,lrs=3,matpmat=4))
#predict() er ligesom normalt, men vi skal angive 's' for tuning-parameteren, dvs. lambda
#jeg v�lger en lav v�rdi, for variablene er valgt teoretisk => ikke s� bange for overfitting
#type="response" er det man angiver i predict() n�r man skal have forudsagt ssh fra en model
predict(lasso,newx=vkmdat,s=.001,type="response")
#70 pct. ssh for pro-EU... sounds about right 