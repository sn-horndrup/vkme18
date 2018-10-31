##WORKSHOP II##

library(pacman)
p_load(tidyverse, janitor)

#Loader datasæt IRIS (indbygget i R)
iris

iris <- iris %>%
  mutate(laengdedummy = ifelse(Petal.Length>4,   1, 0),
           type= case_when(Species== "setosa" ~ "a",
                           Species== "versicolor" ~ "b",
                           TRUE ~ NA_character_))


##ØVELSE##
rm(list=ls())

setwd("~/GitHub/vkme18")
pacman::p_load(tidyverse,broom,rio,stargazer,effects,ggeffects,plm,lmtest, janitor, haven, labelled)

dir("data")


#Importerer de to datsæt
ess8 <- import("data/07_ESS8GB.dta")
  as_tibble()

ess8gb <- import("data/07_ESS8GB_cs.dta") 
  as_tibble()
  
#Merger de to datsæt
# essgb_samlet <- merge(ess8, ess8gb ,by="idno")

##FGH's alternative måde
essgb_fgh <- ess8 %>% 
  left_join(.,ess8gb, by="idno") #beholder var labels


#Kigger variable (FGH's mergede data_frame)
var_label(essgb_fgh$imueclt) ; val_labels(essgb_fgh$imueclt)
var_label(essgb_fgh$eurefvt) ; val_labels(essgb_fgh$eurefvt)
var_label(essgb_fgh$euvthow) ; val_labels(essgb_fgh$euvthow)
var_label(essgb_fgh$euvtifno) ; val_labels(essgb_fgh$euvtifno)


#Laver leavevote variabel
essgb_fgh <- essgb_fgh %>% 
  mutate(immsenrich=ifelse(imueclt<11,imueclt,NA),
         leavevote=case_when(euvthow==2 ~ 1,
                             euvthow==1 ~ 0,
                             euvtifno==2 ~ 1,
                             euvtifno==1 ~ 0,
                             TRUE ~ NA_real_))

#fitter model
m1 <- glm(leavevote~immsenrich,data=essgb_fgh,family="binomial")
stargazer(m1,type="text")

#get predictions
m1preds <- ggpredict(m1,"immsenrich")

#visualize
ggplot(m1preds,aes(x=x,y=predicted,ymin=conf.low,ymax=conf.high)) +
  geom_ribbon(alpha=.3) +
  geom_line() +
  theme_minimal() +
  labs(x="Immigrants enrich culture",y="Pr(vote Leave)")




