setwd("~/GitHub/vkme18/")

pacman::p_load(readr,stargazer,dplyr,coefplot)

### Randomisering i praksis

#1. lad os sige vi har et datas�t med 100 mennesker. 10 skal have treatment

df<-data.frame(id=1:100)
N<-nrow(df)
m<-10

#2. s�t et seed
set.seed(1234)

#3. tilf�ldigt tal for hver person
df$randomnum<-sample(10000,N,replace=F)

#4. sorter iht. det tilf�ldige tal (arrange() kommer fra dplyr)
df<-arrange(df,randomnum)

#5. assign treatment til de f�rst m observationer
df$treat<-0
df$treat[1:m]<-1

#6. for god ordens skyld: sorter tilbage til oprindelig r�kkef�lge
df<-arrange(df,id)

### Gerber, Green & Larimer (2008)

#indl�s data
ggl<-read_csv("data/09_ggl.csv")

#kig p� data
str(ggl)

#regression p� turnout af exp treatment
ols1<-lm(primary2006~messages,data=ggl)
summary(ols1)

#g�r control til referencekategori
ggl$treatmentfac<-relevel(as.factor(ggl$messages),ref="Control")

#igen: regression p� turnout af exp treatment
ols2<-lm(primary2006~treatmentfac,data=ggl)
summary(ols2)

#vis p� tabelform
stargazer(ols2,type="text")

#vis som koefficientplot
coefplot(ols2,intercept=F,horizontal=T,color="black") + theme_bw()

#vigtigt balancecheck: balance p� pre-treatment turnout?
olsbc<-lm(primary2004~treatmentfac,data=ggl)
summary(olsbc)