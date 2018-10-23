setwd("~/GitHub/vkme18")

library(pacman)
p_load(tidyverse, rio, readtext, stringr, quanteda)


###DICTIONARY METODEN###

# 1: importer tekster
nytaars <- readtext('data/nytaarstaler')

# 2: lav korpus
ncorp <- corpus(nytaars)

# 3: importer ordbog (AFINN)
afinn <- import("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-da-32.txt")

str(afinn)

posord <- afinn$V1[afinn$V2 > 0]
negord <- afinn$V1[afinn$V2 < 0]

dict <- dictionary(list(pos=posord,
                        neg=negord))

# 4: lav en dfm (document feature matrix) baseret på ordbogen (dictionary)
ndfm <- dfm(ncorp,dictionary = dict) 

# 5: lav om til data frame
ndf <- quanteda::convert(ndfm,to="data.frame")

##Kigger på summary af ndf
corpsummary <- ncorp %>% 
  summary() %>%
  as_data_frame()

# 6: analyser ordbalance over tid
ndf <- ndf %>% 
  mutate(year=substr(document,1,4),
         balance=pos-neg) %>% 
  left_join(.,corpsummary,by=c("document"="Text")) %>% #fletter corpus summary ind så vi ved hvor mange ord der er i hver tekst 
  mutate(balanceperword=balance/Tokens)

ggplot(ndf,aes(year,balanceperword)) +
  geom_point()

ndf %>% 
  arrange(balance) %>% 
  slice(1:3)



####WORDSCORES: BATURO & MIKHAYLOV####

# 1: importer tekster
bhtxts<-readtext("data/04_bh/2011_2012", encoding="windows-1251") #encoding kan estimeres med encoding()

bhtxts[1,2]
bhtxts[,1]

#definer referencevaerdier
tscores<-c(rep(NA,41),-1,rep(NA,16),1,rep(NA,33))

# 2: Konstruer korpus
bhcorp<-corpus(bhtxts)

# 3: konstruer document feature matrice
bhdfm<-dfm(bhcorp,remove="\\s+",valuetype="regex")

# 4: estimer wordscores
bh_ws<-textmodel_wordscores(bhdfm,tscores)

#konstruer data frame med praedikerede wordscores
bh_ws_pred<-predict(bh_ws) 
bh_ws_df<-data_frame(text=names(bh_ws_pred),score=as.numeric(bh_ws_pred)) %>% 
  mutate(yr=as.numeric(str_extract(text,"[0-9]+")),
         province=str_extract(text,"[^_]+"))

#modeller positioner som fkt af tid
olsmodel<-lm(score~yr+province,data=bh_ws_df)
summary(olsmodel)


##ØVELSE: LAV EGEN DICTIONARY FOR ANALYSE AF NYTÅRSTALER##
nytaars <- readtext('data/nytaarstaler')

# 2: lav korpus
ncorp <- corpus(nytaars)

# 3: definer dictionary
fodboldordbog <- dictionary(list(VejleBoldklub = c("jeppe vierø", "vejle", "vb"), 
                                 FCMidtjylland = c("fcm", "midtjylland", "herning"),
                                 FCKøbenhavn = c("fck", "hovedstaden", "københavn"), 
                                 AaB = c("danmarks paris","aab", "aalborg"),
                                 EsbjergfB = c("esbjerg", "efb","fisk"),
                                 AGF = c("aarhus", "smilets by", "agf"),
                                 Brøndby = c("brøndby", "vestegnen", "bif"),
                                 ACHorsens = c("horsens", "ach"), 
                                 FCNordsjælland = c("peter brixtofte", "farum", "fcn"), 
                                 Sønderjyske = c("ringridning", "sønderjylland", "haderslev"), 
                                 RandersFC = c("mokai", "randers", "rfc"), 
                                 OB = c("h.c. andersen", "odense", "ob"), 
                                 Vendsyssel = c("thy", "vendsyssel", "vff", "skagen"), 
                                 HobroIK = c("quincy antipas", "hobro", "hik")))

#Laver en dfm på baggrund af den store fodbold ordbog
fodbolddfm <- dfm(ncorp,dictionary = fodboldordbog) 

# 5: lav om til data frame
fodbolddf <- quanteda::convert(fodbolddfm,to="data.frame")

#Udregn frekvens - Jeppe er ikke nævnt en eneste gang i hele 72 taler 
frekvens <- colSums(fodbolddf != 0)
frekvens


# 6: plot frekvenser over tid (FH)
fodbolddf_tidy <- fodbolddf %>% 
  mutate(yr=as.numeric(substr(document,1,4))) %>% #Laver yr variabel med numeriske værdier af document for de første 4 tegn
  select(-document) %>%  #fjerne variablen document
  gather(Klub,Omtaler,1:14) #Laver long tabel som tidy

ggplot(fodbolddf_tidy,aes(x=yr,y=Omtaler,group=Klub,color=Klub)) +
  geom_line() +
  scale_color_viridis_d() +
  theme_minimal()

