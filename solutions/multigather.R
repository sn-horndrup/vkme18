pacman::p_load(tidyverse)

#vi laver noget paneldata p� bred form med flere tidsvarierende variable

panel_wide <- data_frame(id=1:4,
                         koen=c("M","K","M","K"),
                         holdning1t1=sample(1:5,4),
                         holdning1t2=sample(1:5,4),
                         holdning2t1=sample(1:5,4),
                         holdning2t2=sample(1:5,4))

#problem: vi vil gerne have det p� lang form

#hvis vi kun havde 1 holdningsvariabel var det nemt nok:

panel_long1 <- panel_wide %>% 
  select(1:4) %>% 
  gather(var,holdning1,3:4)

#... men vi har flere holdningsvariable. det nemmeste er nok at samle id'er og tidsinvariante variable i et langt format
# og s� flette de tidsvarierende varibale p� een ad gangen.

#f�rst laver vi en lang version med id, de tidsinvariante variable (her: k�n), og den f�rste af de tidsvarierende

panel_long1 <- panel_wide %>% 
  select(id,koen,contains("holdning1")) %>% #contains() samler alle de variabelnavne der indeholder en given streng
  gather(tid,holdning1,contains("holdning1")) %>% 
  mutate(tid=str_sub(tid,-2)) #lille ekstra trick: m str_sub() tager vi de sidste 2 tegn af variabelnavnet for at lave en tidsindikator

#s� laver vi en lang version med hver af de tidsvarierende variable og id

panel_long2 <- panel_wide %>% 
  select(id,contains("holdning2")) %>% 
  gather(tid,holdning2,contains("holdning2")) %>% 
  mutate(tid=str_sub(tid,-2)) 

#n�r s� man har gjort det for alle de tidsvarierende variable kan man flette det hele sammen med id og tid

panel_long <- panel_long1 %>% 
  left_join(.,panel_long2,by=c("id","tid"))