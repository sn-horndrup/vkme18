#####SCRAPER FRA TWITTERS REST API################
library(rtweet)
 
#CONSUMER KEY OG SECRET
key<-"WhxWYkUypp7A0NPZoT01eYk3c"
secret<-"W5lvgfMeKnstCVnvf5Qse2FcnsTLUFW44NBklAAvX3jobb4aUY"  

#ACCESS TOKEN KEY & ACCESS TOKEN SECRET
atkey <- "838016372928040960-Bug2FxiU7cbgL33gs64ll1fx0uI5QAB"
atsecret <- "kUQk7Vo5SF07ynXAkeH6UIzkmOUihWNN0hVVk6V8IEbdk"

#LAVER TOKEN
create_token(app = "shorndrup", key, secret, atkey, atsecret)

#HENTER LISTE MED OPSTILLEDE FOLKETINGSKANDIDATER
medlemsliste <- lists_members(slug = "valg-folketinget-2015", owner_user = "ernstpoulsen") 
glimpse(medlemsliste)

#OPDELER I SMÅ BIDDER SÅ DET ER MINDRE DOWNLOADTUNGT AT HENTE TWEETS
list1 <- medlemsliste$user_id[1:100]
list2 <- medlemsliste$user_id[101:200]
list3 <- medlemsliste$user_id[201:300]
list4 <- medlemsliste$user_id[301:400]
list5 <- medlemsliste$user_id[401:450]
list6 <- medlemsliste$user_id[451:512]

#HENTER ALLE KANDIDATERNES SIDSTE 3200 TWEETS (LIMIT HOS TWITTER) UDEN RE-TWEETS
tmls1 <- get_timelines(list1, n = 3200, include_rts = F)
tmls2 <- get_timelines(list2, n = 3200, include_rts = F)
tmls3 <- get_timelines(list3, n = 3200, include_rts = F)
tmls4 <- get_timelines(list4, n = 3200, include_rts = F)
tmls5 <- get_timelines(list5, n = 3200, include_rts = F)
tmls6 <- get_timelines(list6, n = 3200, include_rts = F)


class(tmls1) #KONVERTERET KORREKT TIL EN DATA FRAME

#MERGER DISSE TIL ÉN DATA FRAME
tmls1 <- tmls1 %>% 
  rbind(., tmls2) %>%
  rbind(., tmls3) %>% 
  rbind(., tmls4) %>% 
  rbind(., tmls5) %>% 
  rbind(., tmls6)


save(tmls1, file = "tweets.RData")

#STARTER ANALYSE
rm(list = ls()) #CLEARER TIDLIGERE INDHOLD

setwd("~/GitHub/vkme18/data")
dir()

load('tweets.RData')


#INSTALLERER RELEVANTE PAKKER
library(pacman)
pacman::p_load(tidyverse, lubridate, stringr, tidytext, quanteda, rio, tm, stargazer, rdd)

##########CLEANING TIL DICTIONARY ANALYSE##############

#KOGER NED TIL RELEVANTE VARIABLE
tmls <- tmls1 %>%
  filter(account_created_at <= '2015-05-01 00:00:00') %>% #BEHOLDER KUN BRUGERE, DER HAR EKSISTERET RELATIVT LÆNGE
  filter(is_retweet == F) %>% #SORTERER RE-TWEETS FRA SELVOM DETTE ER GJORT INDLEDNINGSVIS BARE FOR EN SIKKERHEDS SKYLD
  filter(str_detect(text, "#dkpol|#eupol|#uddpol|#dkgreen|#sundpol|#fv15")) %>% #SORTERER ALT DER IKKE HANDLER OM POLITIK FRA
  select(status_id, created_at, screen_name, text, retweet_count, hashtags, name, followers_count, statuses_count) #VÆLGER RELEVANTE VARIABLE


##TILFØJER PARTIVARIABLE TIL DATA FRAME####
tmls <- tmls %>%
  mutate(party = case_when(screen_name == "HavOrla" ~ "Socialdemokratiet",
                           screen_name == "annette_lind" ~ "Socialdemokratiet",
                           screen_name == "JulieSkovsby" ~ "Socialdemokratiet",
                           screen_name == "TorstenGejl" ~ "Alternativet",
                           screen_name == "Kristensenberth" ~ "Dansk Folkeparti",
                           screen_name == "NWammen" ~ "Socialdemokratiet",
                           screen_name == "JakobSoelvhoej" ~ "Enhedslisten",
                           screen_name == "SociologenHD" ~ "Liberal Alliance",
                           screen_name == "SorenPape" ~ "Konservative",
                           screen_name == "AnniMatthiesen" ~ "Venstre",
                           screen_name == "ech58" ~ "Socialdemokratiet",
                           screen_name == "TildeBork" ~ "Dansk Folkeparti",
                           screen_name == "espersendf" ~ "Dansk Folkeparti",
                           screen_name == "jacobmark_sf" ~ "Socialistisk Folkeparti",
                           screen_name == "larsclilleholt" ~ "Venstre",
                           screen_name == "khoenge" ~ "Socialistisk Folkeparti",
                           screen_name == "ChristinaEgelun" ~ "Liberal Alliance",
                           screen_name == "ChristianJuhl2" ~ "Enhedslisten",
                           screen_name == "MarleneHARPSOEE" ~ "Dansk Folkeparti",
                           screen_name == "mfMorten" ~ "Socialdemokratiet",
                           screen_name == "JoachimBOlsen" ~ "Liberal Alliance",
                           screen_name == "olebirkolesen" ~ "Liberal Alliance",
                           screen_name == "MayBrittKattrup" ~ "Liberal Alliance",
                           screen_name == "TrineTorp" ~ "Socialistisk Folkeparti",
                           screen_name == "Antorini123" ~ "Socialdemokratiet",
                           screen_name == "KofodPeter" ~ "Dansk Folkeparti",
                           screen_name == "LisbethBPoulsen" ~ "Socialistisk Folkeparti",
                           screen_name == "Jesper_Pet" ~ "Socialdemokratiet",
                           screen_name == "elfinns" ~ "Enhedslisten",
                           screen_name == "MereteRiisager" ~ "Liberal Alliance",
                           screen_name == "RasmusJarlov" ~ "Konservative",
                           screen_name == "rasmusnordqvist" ~ "Alternativet",
                           screen_name == "MogensJensenS" ~ "Socialdemokratiet",
                           screen_name == "_MaiMercado" ~ "Konservative",
                           screen_name == "martinlidegaard" ~ "Radikale Venstre",
                           screen_name == "AdsbolAdsbl" ~ "Dansk Folkeparti",
                           screen_name == "BrianAMikkelsen" ~ "Konservative",
                           screen_name == "RosaLundEl" ~ "Enhedslisten",
                           screen_name == "PSkipperEL" ~ "Enhedslisten",
                           screen_name == "BendixenPebe" ~ "Dansk Folkeparti",
                           screen_name == "PHummelgaard" ~ "Socialdemokratiet",
                           screen_name == "AneHalsboe" ~ "Socialdemokratiet",
                           screen_name == "LiseBech" ~ "Dansk Folkeparti",
                           screen_name == "sarah_glerup" ~ "Enhedslisten",
                           screen_name == "mattiastesfaye" ~ "Socialdemokratiet",
                           screen_name == "usandbaek" ~ "Alternativet",
                           screen_name == "MFThomasJensen" ~ "Socialdemokratiet",
                           screen_name == "MetteGjerskov" ~ "Socialdemokratiet",
                           screen_name == "BennyEngelbrech" ~ "Socialdemokratiet",
                           screen_name == "DanJoergensen" ~ "Socialdemokratiet",
                           screen_name == "AajaCL" ~ "Global Greenlander",
                           screen_name == "LotteRod" ~ "Radikale Venstre",
                           screen_name == "ThDanielsen" ~ "Venstre",
                           screen_name == "zeniastampe" ~ "Radikale Venstre",
                           screen_name == "Martin_Geertsen" ~ "Venstre",
                           screen_name == "JanEJoergensen" ~ "Venstre",
                           screen_name == "Jens_Joel" ~ "Socialdemokratiet",
                           screen_name == "KirstenNormann" ~ "Socialistisk Folkeparti",
                           screen_name == "kplorentzen" ~ "Venstre",
                           screen_name == "jacobjensenMF" ~ "Venstre",
                           screen_name == "Torstenschack" ~ "Venstre",
                           screen_name == "JakobEllemann" ~ "Venstre",
                           screen_name == "lauralindahl" ~ "Liberal Alliance",
                           screen_name == "Trinebramsen" ~ "Socialdemokratiet",
                           screen_name == "pelledragsted" ~ "Enhedslisten",
                           screen_name == "AStjernholm" ~ "Alternativet",
                           screen_name == "lykketoft" ~ "Socialdemokratiet",
                           screen_name == "JosephineFock" ~ "Alternativet",
                           screen_name == "uffeelbaek" ~ "Alternativet",
                           screen_name == "KaareDybvad" ~ "Socialdemokratiet",
                           screen_name == "a_steenberg" ~ "Radikale Venstre",
                           screen_name == "PiaOlsen" ~ "Socialistisk Folkeparti",
                           screen_name == "CarlHolst" ~ "Venstre",
                           screen_name == "Astridkrag" ~ "Socialdemokratiet",
                           screen_name == "mettereissmann" ~ "Socialdemokratiet",
                           screen_name == "mettebock" ~ "Liberal Alliance",
                           screen_name == "NikolajAmstrup" ~ "Alternativet",
                           screen_name == "KarinGaardsted" ~ "Socialdemokratiet",
                           screen_name == "MagniArge" ~ "Tjóðveldi",
                           screen_name == "flydtkjaer" ~ "Dansk Folkeparti",
                           screen_name == "RosenkrantzT" ~ "Socialdemokratiet",
                           screen_name == "Hans_Kr_Skibby" ~ "Dansk Folkeparti",
                           screen_name == "PAdelsteen" ~ "Dansk Folkeparti",
                           screen_name == "MortenMarinus" ~ "Dansk Folkeparti",
                           screen_name == "LeaWermelin" ~ "Socialdemokratiet",
                           screen_name == "PanduroMaja" ~ "Socialdemokratiet",
                           screen_name == "SosseSass" ~ "Socialdemokratiet",
                           screen_name == "RabjergMadsen" ~ "Socialdemokratiet",
                           screen_name == "AJohanssonC" ~ "Konservative",
                           screen_name == "JerkelK" ~ "Konservative",
                           screen_name == "RavnTroels" ~ "Socialdemokratiet",
                           screen_name == "FlemmingMM" ~ "Socialdemokratiet",
                           screen_name == "VillumC" ~ "Liberal Alliance",
                           screen_name == "naserkhaderdk" ~ "Konservative",
                           screen_name == "HansAndersenV" ~ "Venstre",
                           screen_name == "LahnLeif" ~ "Socialdemokratiet",
                           screen_name == "mariannejelved" ~ "Radikale Venstre",
                           screen_name == "brittbager" ~ "Venstre",
                           screen_name == "dfmehd_mette" ~ "Dansk Folkeparti",
                           screen_name == "mikkelsen_leif" ~ "Liberal Alliance",
                           screen_name == "SSkaale" ~ "Javnaðarflokkurin",
                           screen_name == "ToftJakobsen" ~ "Socialdemokratiet",
                           screen_name == "Mikkel_Dencker" ~ "Dansk Folkeparti",
                           screen_name == "PeterJuelJensen" ~ "Venstre",
                           screen_name == "JThulesen" ~ "Dansk Folkeparti",
                           screen_name == "KarenJKlint" ~ "Socialdemokratiet",
                           screen_name == "DfkichKim" ~ "Dansk Folkeparti",
                           screen_name == "BHaarder" ~ "Venstre",
                           screen_name == "EvaKjerHansen" ~ "Venstre",
                           screen_name == "Eilersen_DF" ~ "Dansk Folkeparti",
                           screen_name == "AleqaHammond" ~ "Nunatta Qitornai",
                           screen_name == "EvaFlyvholm" ~ "Enhedslisten",
                           screen_name == "skaarup_df" ~ "Dansk Folkeparti",
                           screen_name == "sfhoni" ~ "Socialistisk Folkeparti",
                           screen_name == "ClausKvist_DF" ~ "Dansk Folkeparti",
                           screen_name == "troelslundp" ~ "Venstre",
                           screen_name == "MarieKrarup" ~ "Dansk Folkeparti",
                           screen_name == "EllemannKaren" ~ "Venstre",
                           screen_name == "ErlingBonnesen" ~ "Venstre",
                           screen_name == "KarinaDue1" ~ "Dansk Folkeparti",
                           screen_name == "ThomasVesth" ~ "Liberal Alliance",
                           screen_name == "JTPAlternativet" ~ "Alternativet",
                           screen_name == "VibekeGenefke" ~ "Alternativet",
                           screen_name == "HansHenrikDF" ~ "Dansk Folkeparti",
                           screen_name == "qasamnahmad" ~ "Alternativet",
                           screen_name == "PerBFrostholm" ~ "Liberal Alliance",
                           screen_name == "RuneLundgaard" ~ "Alternativet",
                           screen_name == "AnkerSchjerning" ~ "Enhedslisten",
                           screen_name == "9000konservativ" ~ "Konservative",
                           screen_name == "MariaTemponeras" ~ "Enhedslisten",
                           screen_name == "stem_thomaskrog" ~ "Socialistisk Folkeparti",
                           screen_name == "RolfBjerre" ~ "Alternativet",
                           screen_name == "BidstrupM" ~ "Alternativet",
                           screen_name == "Fri_Og_Frank" ~ "Liberal Alliance",
                           screen_name == "LeaJLie1" ~ "Alternativet",
                           screen_name == "kudsken_" ~ "Dansk Folkeparti",
                           screen_name == "miv2711" ~ "Liberal Alliance",
                           screen_name == "JaneAlternative" ~ "Alternativet",
                           screen_name == "BinaSeff" ~ "Alternativet",
                           screen_name == "mikidamlarsen" ~ "Socialdemokratiet",
                           screen_name == "rvhenriette" ~ "Radikale Venstre",
                           screen_name == "WesterholtDan" ~ "Alternativet",
                           screen_name == "HjortStorm" ~ "Dansk Folkeparti",
                           screen_name == "Ammitzboell_K" ~ "Konservative",
                           screen_name == "Bille4U" ~ "Dansk Folkeparti",
                           screen_name == "hildeman91" ~ "Liberal Alliance",
                           screen_name == "NedergaardTina" ~ "Venstre",
                           screen_name == "jenshenrikkirk" ~ "Konservative",
                           screen_name == "Knuddla" ~ "Liberal Alliance",
                           screen_name == "FridtjofKD" ~ "Kristendemokraterne",
                           screen_name == "a_vilhelmsen" ~ "Socialistisk Folkeparti",
                           screen_name == "smpihl" ~ "Konservative",
                           screen_name == "MathiasBrosboel" ~ "Kristendemokraterne",
                           screen_name == "AnahitaKBH" ~ "Venstre",
                           screen_name == "jacobrabjerg" ~ "Kristendemokraterne",
                           screen_name == "AnpPedersen" ~ "Socialistisk Folkeparti",
                           screen_name == "RasmusVMadsen" ~ "Enhedslisten",
                           screen_name == "HellwingJacob" ~ "Kristendemokraterne",
                           screen_name == "KennethFC_2650" ~ "Socialdemokratiet",
                           screen_name == "KentHolst" ~ "Enhedslisten",
                           screen_name == "StrJesper" ~ "Dansk Folkeparti",
                           screen_name == "FlemmingStenild" ~ "Socialistisk Folkeparti",
                           screen_name == "anbolvig" ~ "Liberal Alliance",
                           screen_name == "FrankTronborg" ~ "Socialistisk Folkeparti",
                           screen_name == "ckbech1" ~ "Radikale Venstre",
                           screen_name == "KeldLorenzen" ~ "Liberal Alliance",
                           screen_name == "Schnell_Lauri" ~ "Socialdemokratiet",
                           screen_name == "HHowardsen" ~ "Kristendemokraterne",
                           screen_name == "JonathanSimmel" ~ "Enhedslisten",
                           screen_name == "EjnarSchultz" ~ "Venstre",
                           screen_name == "Cspr_Hedegaard" ~ "Radikale Venstre",
                           screen_name == "HelleMolvig" ~ "Radikale Venstre",
                           screen_name == "RRavnskov" ~ "Socialistisk Folkeparti",
                           screen_name == "RuneWingaard" ~ "Alternativet",
                           screen_name == "Kristensenberth" ~ "Dansk Folkeparti",
                           screen_name == "mckusk" ~ "Liberal Alliance",
                           screen_name == "metafuglsang" ~ "Socialistisk Folkeparti",
                           screen_name == "stemkristian" ~ "Venstre",
                           screen_name == "Anne_ThomasDK" ~ "Alternativet",
                           screen_name == "MarleneAmbo" ~ "Venstre",
                           screen_name == "SNhede" ~ "Alternativet",
                           screen_name == "MariaGjerding" ~ "Enhedslisten",
                           screen_name == "karinaRjessen" ~ "Venstre",
                           screen_name == "lynggaardj" ~ "Radikale Venstre",
                           screen_name == "Olehaek" ~ "Socialdemokratiet",
                           screen_name == "FejoeMichele" ~ "Liberal Alliance",
                           screen_name == "ClausHouden" ~ "Venstre",
                           screen_name == "BuchJonna" ~ "Radikale Venstre",
                           screen_name == "dam_helle" ~ "Socialdemokratiet",
                           screen_name == "Klausmygind" ~ "Socialistisk Folkeparti",
                           screen_name == "HelleMolvig" ~ "Radikale Venstre",
                           screen_name == "1953PoulErik" ~ "Socialistisk Folkeparti",
                           screen_name == "MiguelHobro" ~ "Enhedslisten",
                           screen_name == "Flipitorflop" ~ "Liberal Alliance",
                           screen_name == "Ryom_92" ~ "Socialdemokratiet",
                           screen_name == "steenHiversen" ~ "Liberal Alliance",
                           screen_name == "AlbrechtsenMaja" ~ "Enhedslisten",
                           screen_name == "LBarfoed" ~ "Konservative",
                           screen_name == "GertBjerregaard" ~ "Venstre",
                           screen_name == "kirstensloth" ~ "Socialistisk Folkeparti",
                           screen_name == "AnnaMHC" ~ "Dansk Folkeparti",
                           screen_name == "sfBalder" ~ "Socialistisk Folkeparti",
                           screen_name == "HoghSorensen" ~ "Dansk Folkeparti",
                           screen_name == "GitteMadsen4" ~ "Radikale Venstre",
                           screen_name == "AndersLadegaar1" ~ "Dansk Folkeparti",
                           screen_name == "KarinaBergmann1" ~ "Liberal Alliance",
                           screen_name == "JesperBraemerLA" ~ "Liberal Alliance",
                           screen_name == "KSkarsholm" ~ "Kristendemokraterne",
                           screen_name == "MarkGrossmannV" ~ "Venstre",
                           screen_name == "PBoisen" ~ "Enhedslisten",
                           screen_name == "FlemmingDL" ~ "Venstre",
                           screen_name == "TClausen_" ~ "Enhedslisten",
                           screen_name == "ole_beck" ~ "Socialdemokratiet",
                           screen_name == "MartinMlbak" ~ "Socialdemokratiet",
                           screen_name == "J_Rixen" ~ "Liberal Alliance",
                           screen_name == "Heidi_S_Rud" ~ "Enhedslisten",
                           screen_name == "OmerCiftci54" ~ "Venstre",
                           screen_name == "LailaLauridsen" ~ "Socialistisk Folkeparti",
                           screen_name == "MikeLegarthC" ~ "Konservative",
                           screen_name == "signe_munk" ~ "Socialistisk Folkeparti",
                           screen_name == "Lisem_ller" ~ "Socialistisk Folkeparti",
                           screen_name == "CKissmeyer" ~ "Venstre",
                           screen_name == "jacobklivager" ~ "Socialistisk Folkeparti",
                           screen_name == "cekicozlem" ~ "Socialistisk Folkeparti",
                           screen_name == "rasmushelveg" ~ "Radikale Venstre",
                           screen_name == "DonkinBente" ~ "Socialistisk Folkeparti",
                           screen_name == "PerNansted" ~ "Kristendemokraterne",
                           screen_name == "JimmySteenE" ~ "Liberal Alliance",
                           screen_name == "PeterMikkelsen4" ~ "Enhedslisten",
                           screen_name == "KisserFLehnert" ~ "Liberal Alliance",
                           screen_name == "SommerHelle" ~ "Radikale Venstre",
                           screen_name == "JulieG0ttschalk" ~ "Dansk Folkeparti",
                           screen_name == "Kasperliberal" ~ "Liberal Alliance",
                           screen_name == "SisseMarieWe" ~ "Socialistisk Folkeparti",
                           screen_name == "PerZeidler" ~ "Liberal Alliance",
                           screen_name == "PallesenLars" ~ "Alternativet",
                           screen_name == "jnkjaer" ~ "Alternativet",
                           screen_name == "rv_sean" ~ "Radikale Venstre",
                           screen_name == "HelleRadikale" ~ "Radikale Venstre",
                           screen_name == "TorbenAntonHans" ~ "Enhedslisten",
                           screen_name == "AghaRazvi" ~ "Radikale Venstre",
                           screen_name == "TPalmskov" ~ "Dansk Folkeparti",
                           screen_name == "Niller59" ~ "Kristendemokraterne",
                           screen_name == "increase_nick" ~ "Venstre",
                           screen_name == "PerssonJess" ~ "Dansk Folkeparti",
                           screen_name == "TommyHoje" ~ "Liberal Alliance",
                           screen_name == "Avedgren" ~ "Radikale Venstre",
                           screen_name == "NyborgJohn" ~ "Socialdemokratiet",
                           screen_name == "perleberger" ~ "Alternativet",
                           screen_name == "ThiesenMette" ~ "Konservative",
                           screen_name == "Anne_Sina" ~ "Socialdemokratiet",
                           screen_name == "UllaBirkJ" ~ "Socialistisk Folkeparti",
                           screen_name == "StigGrenov" ~ "Kristendemokraterne",
                           screen_name == "BlemClausen" ~ "Alternativet",
                           screen_name == "LeifDonbaek" ~ "Socialistisk Folkeparti",
                           screen_name == "JakobLavrsen" ~ "Radikale Venstre",
                           screen_name == "leth_wad" ~ "Radikale Venstre",
                           screen_name == "JBitzow" ~ "Konservative",
                           screen_name == "KHegaard" ~ "Radikale Venstre",
                           screen_name == "MortenDahlin" ~ "Radikale Venstre",
                           screen_name == "BennyDall" ~ "Enhedslisten",
                           screen_name == "RasmusJarlov" ~ "Konservative",
                           screen_name == "RPHenriksen" ~ "Socialistisk Folkeparti",
                           screen_name == "BjarkeCharlie" ~ "Alternativet",
                           screen_name == "GlenMadsen" ~ "Dansk Folkeparti",
                           screen_name == "kaaretraberg" ~ "Alternativet",
                           screen_name == "GithaNelander" ~ "Dansk Folkeparti",
                           screen_name == "frederikd10" ~ "Liberal Alliance",
                           screen_name == "ErikFischerDK" ~ "Alternativet",
                           screen_name == "abir1206" ~ "Radikale Venstre",
                           screen_name == "MF_K_Lorentzen" ~ "Socialistisk Folkeparti",
                           screen_name == "sineheltberg" ~ "Socialdemokratiet",
                           screen_name == "RasmusKSlot" ~ "Enhedslisten",
                           screen_name == "ThomasRisum" ~ "Alternativet",
                           screen_name == "jaclund" ~ "Socialdemokratiet",
                           screen_name == "Paulin_Anne" ~ "Socialdemokratiet",
                           screen_name == "JeppeTrolle" ~ "Radikale Venstre",
                           screen_name == "MadsHolger" ~ "Konservative",
                           screen_name == "gertingolf" ~ "Dansk Folkeparti",
                           screen_name == "SophieDanneris" ~ "Radikale Venstre",
                           screen_name == "bro_lars" ~ "Liberal Alliance",
                           screen_name == "BASkotte" ~ "Venstre",
                           screen_name == "Carl__Valentin" ~ "Socialistisk Folkeparti",
                           screen_name == "VibekeNorly" ~ "Venstre",
                           screen_name == "elisabeth_ildal" ~ "Liberal Alliance",
                           screen_name == "TinneBorch" ~ "Venstre",
                           screen_name == "FKenneth" ~ "Dansk Folkeparti",
                           screen_name == "JohnDyrbyPaulse" ~ "Socialdemokratiet",
                           screen_name == "JKofoedC" ~ "Konservative",
                           screen_name == "AndersFKnudsen" ~ "Kristendemokraterne",
                           screen_name == "jan_kjr" ~ "Socialistisk Folkeparti",
                           screen_name == "sorenvanting" ~ "Konservative",
                           screen_name == "MFThomasJensen" ~ "Socialdemokratiet",
                           screen_name == "SvendThorhauge" ~ "Radikale Venstre",
                           screen_name == "c_scharling" ~ "Radikale Venstre",
                           screen_name == "troelsbrandt1" ~ "Radikale Venstre",
                           screen_name == "Helle_Bonnesen" ~ "Konservative",
                           screen_name == "Soren_Juliussen" ~ "Liberal Alliance",
                           screen_name == "b_juul" ~ "Socialistisk Folkeparti",
                           screen_name == "O76kocak" ~ "Socialdemokratiet",
                           screen_name == "IsabellaArendt" ~ "Kristendemokraterne",
                           screen_name == "engelschmidt" ~ "Venstre",
                           screen_name == "andpourkamali" ~ "Radikale Venstre",
                           screen_name == "sannebjoern" ~ "Radikale Venstre",
                           screen_name == "Casperstrunge" ~ "Konservative",
                           screen_name == "FruToxic" ~ "Enhedslisten",
                           screen_name == "kjaer_kjaer" ~ "Alternativet",
                           screen_name == "dennispaaske" ~ "Liberal Alliance",
                           screen_name == "MortenDreyerDF" ~ "Dansk Folkeparti",
                           screen_name == "StemChristian" ~ "Konservative",
                           screen_name == "tomblock77" ~ "Socialdemokratiet",
                           screen_name == "pernillebk" ~ "Radikale Venstre",
                           screen_name == "bjarne_a" ~ "Socialdemokratiet",
                           screen_name == "LouiseElholm" ~ "Venstre",
                           screen_name == "nielswanning" ~ "Radikale Venstre",
                           screen_name == "mathiasvjessen" ~ "Radikale Venstre",
                           screen_name == "LoneLoklindt" ~ "Radikale Venstre",
                           screen_name == "jesperkiel" ~ "Enhedslisten",
                           screen_name == "BirgitteKP" ~ "Socialistisk Folkeparti",
                           screen_name == "Broecher" ~ "Radikale Venstre",
                           screen_name == "helgevagn" ~ "Radikale Venstre",
                           screen_name == "danielnyboe" ~ "Radikale Venstre",
                           screen_name == "KasperNordborgK" ~ "Socialistisk Folkeparti",
                           screen_name == "trinemach" ~ "Socialistisk Folkeparti",
                           screen_name == "jensensander" ~ "Alternativet",
                           screen_name == "LasseSF" ~ "Socialistisk Folkeparti",
                           screen_name == "BOSTMI" ~ "Alternativet",
                           screen_name == "Perhusted" ~ "Socialdemokraterne",
                           screen_name == "AnetteSkafte" ~ "Dansk Folkeparti",
                           screen_name == "JeppeBruus" ~ "Socialdemokratiet",
                           screen_name == "SamiraNawa" ~ "Radikale Venstre",
                           screen_name == "nikogrunfeld" ~ "Alternativet",
                           screen_name == "Loekkegaard_MEP" ~ "Venstre",
                           screen_name == "KleinschmidtDK" ~ "Radikale Venstre",
                           screen_name == "MalteLarsen" ~ "Socialdemokratiet",
                           screen_name == "runekdk" ~ "Konservative",
                           screen_name == "Kimhyttel" ~ "Alternativet",
                           screen_name == "VictoriaRV" ~ "Radikale Venstre",
                           screen_name == "AnnikaHNielsen" ~ "Enhedslisten",
                           screen_name == "akmelvig" ~ "Venstre",
                           screen_name == "rugholm" ~ "Konservative",
                           screen_name == "mettereissmann" ~ "Socialdemokratiet",
                           screen_name == "KlausHjulmand" ~ "Liberal Alliance",
                           screen_name == "fatmaoektem" ~ "Venstre",
                           screen_name == "Graff1996" ~ "Enhedslisten",
                           screen_name == "JensSalling" ~ "Konservative",
                           screen_name == "buchvaldt" ~ "Socialdemokratiet",
                           screen_name == "simonkollerup" ~ "Socialdemokratiet",
                           screen_name == "Blixt22" ~ "Dansk Folkeparti",
                           screen_name == "amgeisler" ~ "Radikale Venstre",
                           screen_name == "nadeemfa" ~ "Radikale Venstre",
                           screen_name == "ChristianPoll" ~ "Alternativet",
                           screen_name == "MikkelLundemann" ~ "Konservative",
                           screen_name == "fightingheart" ~ "Alternativet",
                           screen_name == "simonsjsimonsen" ~ "Socialdemokratiet",
                           screen_name == "sophieloehde" ~ "Venstre",
                           screen_name == "Kielsholm" ~ "Enhedslisten",
                           screen_name == "SuperSejeJanK" ~ "Alternativet",
                           screen_name == "cnc2624" ~ "Enhedslisten",
                           screen_name == "andersbroholm" ~ "Venstre",
                           screen_name == "rasmushorn" ~ "Socialdemokratiet",
                           screen_name == "marleneBL" ~ "Radikale Venstre",
                           screen_name == "metteabildgaard" ~ "Konservative",
                           screen_name == "anderswolf" ~ "Socialistisk Folkeparti",
                           screen_name == "madsfuglede" ~ "Venstre",
                           screen_name == "nvillumsen" ~ "Enhedslisten",
                           screen_name == "YildizAkdogan" ~ "Socialdemokratiet",
                           screen_name == "EllenTraneNorby" ~ "Venstre",
                           screen_name == "smbrix" ~ "Enhedslisten",
                           screen_name == "Simonpihl" ~ "Socialdemokratiet",
                           screen_name == "Kirstenbrosbol" ~ "Socialdemokratiet",
                           screen_name == "larsloekke" ~ "Venstre",
                           screen_name == "SimonEmilAmmitz" ~ "Liberal Alliance",
                           screen_name == "StemLAURITZEN" ~ "Venstre",
                           screen_name == "anderssamuelsen" ~ "Liberal Alliance",
                           screen_name == "Grantzau" ~ "Alternativet",
                           screen_name == "Uzma2200" ~ "Alternativet",
                           screen_name == "TeisVolstrup" ~ "Socialistisk Folkeparti",
                           screen_name == "claus_hermansen" ~ "Liberal Alliance",
                           screen_name == "PernilleVermund" ~ "Konservative",
                           screen_name == "CamillaHersom" ~ "Radikale Venstre",
                           screen_name == "JaneHeitmann" ~ "Venstre",
                           screen_name == "poulvjensen" ~ "Liberal Alliance",
                           screen_name == "samina0308" ~ "Konservative",
                           screen_name == "annika_smith" ~ "Socialistisk Folkeparti",
                           screen_name == "oestergaard" ~ "Radikale Venstre",
                           screen_name == "dkmalkowski" ~ "Liberal Alliance",
                           screen_name == "michaelaastrup" ~ "Venstre",
                           screen_name == "Kristian_Jensen" ~ "Venstre",
                           screen_name == "sorenpind" ~ "Venstre",
                           screen_name == "MadsRorvig" ~ "Venstre",
                           screen_name == "Toerning" ~ "Dansk Folkeparti",
                           screen_name == "DennisOersted" ~ "Socialistisk Folkeparti",
                           screen_name == "sofiecn" ~ "Radikale Venstre",
                           screen_name == "MiaNyegaard" ~ "Radikale Venstre",
                           screen_name == "JonathanNielsen" ~ "Liberal Alliance",
                           screen_name == "A_Albertsen" ~ "Socialistisk Folkeparti",
                           screen_name == "karmel80" ~ "Radikale Venstre",
                           screen_name == "Paintshooter" ~ "Liberal Alliance",
                           screen_name == "Mikkeldhansen" ~ "Socialistisk Folkeparti",
                           screen_name == "KristianWestfal" ~ "Socialdemokratiet",
                           screen_name == "RasmusBoserup" ~ "Liberal Alliance",
                           screen_name == "HenrikSoerensen" ~ "Konservative",
                           screen_name == "jacobfuglsang" ~ "Alternativet",
                           screen_name == "Heunicke" ~ "Socialdemokratiet",
                           screen_name == "RasmusPrehn" ~ "Socialdemokratiet",
                           screen_name == "nielswesty" ~ "Liberal Alliance",
                           screen_name == "LivHA" ~ "Radikale Venstre",
                           screen_name == "carolinamaier" ~ "Alternativet",
                           screen_name == "Scheelsbeck" ~ "Konservative",
                           screen_name == "Pernilleschnoor" ~ "Alternativet",
                           screen_name == "CharlotteBircow" ~ "Liberal Alliance",
                           screen_name == "agenturet" ~ "Dansk Folkeparti",
                           screen_name == "NeilBloem" ~ "Socialistisk Folkeparti",
                           screen_name == "Giajenthiran" ~ "Radikale Venstre",
                           screen_name == "LisbethBech" ~ "Socialistisk Folkeparti",
                           screen_name == "JohannesLebech" ~ "Radikale Venstre",
                           screen_name == "camillakampmann" ~ "Radikale Venstre",
                           screen_name == "nilasnilas" ~ "Alternativet",
                           screen_name == "manusareen" ~ "Socialdemokratiet",
                           screen_name == "IdaAuken" ~ "Radikale Venstre",
                           screen_name == "klausmarkussen" ~ "Venstre",
                           screen_name == "jens_wenzel" ~ "Enhedslisten",
                           screen_name == "vhellmann" ~ "Liberal Alliance",
                           screen_name == "Malou_Socialdem" ~ "Socialdemokratiet",
                           screen_name == "Berings" ~ "Alternativet",
                           screen_name == "fredeskaaning" ~ "Socialdemokratiet",
                           screen_name == "Kennith_Nielsen" ~ "Alternativet",
                           screen_name == "AMWintherC" ~ "Konservative",
                           screen_name == "Kennith_Nielsen" ~ "Alternativet",
                           screen_name == "DitteBrondum" ~ "Socialdemokratiet",
                           screen_name == "xlokken" ~ "Konservative",
                           screen_name == "perbisgaard" ~ "Venstre",
                           screen_name == "halime9" ~ "Socialistisk Folkeparti",
                           screen_name == "KirstinevanSabb" ~ "Konservative",
                           screen_name == "HolmerHans" ~ "Socialistisk Folkeparti",
                           screen_name == "lyngek" ~ "Alternativet",
                           screen_name == "annehegelund89" ~ "Dansk Folkeparti",
                           screen_name == "DanArnloev" ~ "Konservative",
                           screen_name == "KnoopElse" ~ "Venstre",
                           screen_name == "ErikaLorentsen" ~ "Radikale Venstre",
                           screen_name == "JanSalskov1" ~ "Socialdemokratiet",
                           screen_name == "KjeldLundager" ~ "Radikale Venstre",
                           screen_name == "BragMogens" ~ "Venstre",
                           screen_name == "AVedgren" ~ "Radikale Venstre",
                           screen_name == "JettePlesnerDal" ~ "Dansk Folkeparti",
                           screen_name == "clj1953" ~ "Socialdemokratiet",
                           screen_name == "kasperliberal" ~ "Liberal Alliance",
                           screen_name == "Chresten83" ~ "Dansk Folkeparti",
                           screen_name == "TSteifel" ~ "Konservative",
                           screen_name == "Bille4u" ~ "Dansk Folkeparti",
                           screen_name == "KMBidstrup" ~ "Alternativet",
                           screen_name == "JPTAlternativet" ~ "Alternativet",
                           screen_name == "bostrup1975" ~ "Dansk Folkeparti",
                           TRUE ~ NA_character_))
  

#INDLÆSER DOWNLOADEDE STOPWORDS (TIL BRUG I CLEANING)####
stopwords <- read.delim('da_stopwords.txt')

#CLEANER TWEETS
tmls$text.clean <- tmls$text %>%
  str_replace_all('https://t.co/[a-z,A-Z,0-9]*', '') %>% #FJERNER URL's
  str_replace_all('http://t.co/[a-z,A-Z,0-9]*', '') %>% 
  str_replace_all('\\_', '') %>% #FJERNER UNDERSCORES I SCREEN NAMES 
  #PRØVEDE AT FJERNE @TAGS MED DETTE WHILE LOOP:
  #  while('@' %in% tmls1$text.clean) {
  #    str_replace('\\@[A-Z,a-z]*' , '')
  #  }
  #  MEN KUNNE IKKE FÅ DET TIL AT VIRKE, SÅ MÅTTE TY TIL LIDT TRÆGE NEDENSTÅENDE TILGANG: 
  str_replace('\\@[A-Z,a-z]*' , '') %>%  
  str_replace('\\@[A-Z,a-z]*' , '') %>% #FJERNER SCREEN NAMES
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>%
  str_replace('\\@[A-Z,a-z]*' , '') %>% 
  str_replace('\\#[A-Z,a-z]*', '') %>% #FJERNER HASHTAGS
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace('\\#[A-Z,a-z]*', '') %>%
  str_replace_all('[[:punct:]]', '') %>% #FJERNER ALLE YDERLIGERE TEGN
  str_remove_all('[[:digit:]]') %>% #FJERNER ALLE TAL
  tolower() %>% #GØR ALLE BOGSTAVER SMÅ SÅ SAMME ORD IKKE OPFATTES SOM FORSKELLIGE
  removeWords(.,stopwords$stopord) #FJERNER STOPWORDS FRA STRING VEKTOR "STOPWORDS"

 
#######DICTIONARY ANALYSE MED QUANTEDA########

###IMPORTERER AFINN DICTIONARY TIL KLASSIFIKATION AF NEGATIVE/POSITIVE DANSKE ORD
afinn <- import("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-da-32.txt")


#LAVER ORDBOG MED POSITIVE OG NEGTIVE ORD
positiv <- afinn$V1[afinn$V2 > 0] #POSITIVE ORD ER ORD (V1) MED VÆRDI > 0 (V2)
negativ <- afinn$V1[afinn$V2 < 0] #NEGATIVE ORD ER ORD (V1) MED VÆRDI < 0 (V2)

dict <- dictionary(list(pos=positiv,
                        neg=negativ))

colnames(afinn) <- gsub("V1", "feature", colnames(afinn)) #ÆNDRER KOLONNENAVNET 'V1' TIL 'FEATURE'

#LAVER CORPUS, TOKENS OG DOCUMENT-FEATURE-MATRIX FOR TWEETS
corpus <- corpus(tmls$text.clean)
docvars(corpus, "party") <- tmls$party #TILFØJER PARTIVARIABLEN TIL CORPUS
summary(corpus, 5)

#TOKENIZER CORPUS OG SIKRER AT ALT OVERFLØDIGT ER FJERNET EN EKSTRA GANG
tok <- tokens(corpus, remove_numbers = T, remove_punct = T)

#OPTÆLLER ANTAL ORD I HVER TWEET FRA CORPUS SOM KØRES IGENNEM DICTIONARY 
tn <- tok %>% 
  ntoken() %>% 
  as_data_frame()

colnames(tn) <- gsub("value", "tokens", colnames(tn)) #ÆNDRER KOLONNENAVNET 'VALUE' TIL 'TOKENS'

#LAVER DOCUMENT-FEATURE-MATRIX BASERET PÅ MIN ORDBOG
twdfm <- dfm(tok, dictionary = dict)

#LAVER OGSÅ DOCUMENT-FEATURE-MATRIX UDEN ORDBOG TIL FREKVENSANALYSE AF AFINN ORD####
twdfm_wo <- dfm(corpus)

#UDREGNER FREKVENSFORDELING AF AFINN ORD I ALT
freq_plot <- twdfm_wo %>%
  textstat_frequency(n = 1000) %>% 
  inner_join(afinn, by = "feature") 

colnames(freq_plot) <- gsub("V2", "Score", colnames(freq_plot))

#PLOTTER FREKVENSFORDELING
ggplot(freq_plot[1:23, ], aes(x = reorder(feature, frequency), y = frequency, color = Score)) +
  geom_point(size = 3) + 
  coord_flip() +
  labs(x = NULL, y = "AFINN ordfrekvens: 23 hyppigste ord") +
  theme_minimal()

#KONTEKSTTJEK AF ORDENE 'AFTALE', 'BETALE', 'KRAV' OG 'SIKRE'
kontekst_betale <- kwic(tok, pattern = "betale")
head(kontekst_betale)

kontekst_aftale <- kwic(tok, pattern = "aftale")
head(kontekst_aftale)

kontekst_krav <- kwic(tok, pattern = "krav")
head(kontekst_krav)

kontekst_sikre <- kwic(tok, pattern = "sikre")
head(kontekst_sikre)

#GØR DET SAMME BARE FOR HVERT PARTI
twdfm_wo_party <- twdfm_wo %>% 
  dfm_group(groups = "party")
 
freq_plot_party <- twdfm_wo_party %>%
  textstat_frequency(n = 100, groups = "party") %>% 
  inner_join(afinn, by = "feature")

colnames(freq_plot_party) <- gsub("V2", "Score", colnames(freq_plot_party))

ggplot(freq_plot_party, aes(x = reorder(feature, frequency), y = frequency, color = Score)) +
  geom_point(size = 2) + 
  facet_wrap(~ group) +
  coord_flip() +
  labs(x = NULL, y = "AFINN ordfrekvens") +
  theme_minimal()

###LAVER CORPUS FOR HASHTAGS####
corpustext <- corpus(tmls$text)
docvars(corpustext, "Party") <- tmls$party

dfmtext_party <- dfm(corpustext, select = "#*")

dfmtext_party <- dfmtext_party %>% 
  dfm_group(groups = "Party") %>% 
  dfm_weight(scheme = "prop")

freq_plot <- dfmtext_party %>%
  textstat_frequency(., n = 10, groups = "Party")

# PLOTTER HASHTAG FREQUENCIES
ggplot(data = freq_plot, aes(x = nrow(freq_plot):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_plot):1,
                     labels = freq_plot$feature) +
  labs(x = NULL, y = "Relative frekvenser")


#LAVER DOCUMENT-FEATURE-MATRIX MED AFINN ORDBOG OM TIL EN DATA FRAME####
twdf <- quanteda::convert(twdfm,to="data.frame")

twdf <- twdf %>% 
  mutate(polaritet = pos - neg) %>% #LAVER EN ORDBALANCE MELLEM POSITIVE OG NEGATIVE ORD
  cbind(., tn) #FLETTER ANTAL ORD I HVER TWEET IND: "TOKEN"

twdf <- twdf %>% 
  mutate(balanceperord = polaritet / tokens) #LAVER ORDBALANCE IFT. ANTAL ORD I HVER TWEET
 
twdf <- twdf %>% 
  cbind(., tmls) #FLETTER DEN ORIGINALE DATA FRAME IND

#LAVER CREATED_AT KOLONNE OM TIL R DATO FORMAT
twdf$created_at <- as.Date(twdf$created_at)
#LAVER DATO FORMAT OM TIL NUMERISK VEKTOR
twdf$created_at_num <- as.numeric(twdf$created_at)

#FJERNER OBSERVATIONER HVOR DER IKKE ER BLEVET IDENTIFICERET NOGLE ORD
twdf <- twdf %>%
  na.omit(balanceperord)

#PLOTTER UDVIKLINGEN I ANAL POSITIVE OG NEGATIVE ORD 
udvik <- twdf %>%
  group_by(created_at) %>%
  select(created_at, balanceperord, party) %>% 
  mutate(mean = mean(balanceperord))

#PLOTTER PÅ TVÆRS AF ALLE PARTIER
ggplot(udvik, aes(x = created_at, y = mean)) +
  geom_line(color = "#0072B2") +
  scale_x_date(limits = as.Date(c("2012-01-01", "2018-12-31")),
               date_breaks = "1 year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  xlab("Dato") +
  ylab("Gennemsnitlig Tweetscore (2012-2018)") +
  theme_minimal()

#PLOTTER FOR ALLE PARTIER
ggplot(udvik, aes(x = created_at, y = mean)) +
  geom_line(color = "#0072B2") +
  facet_wrap(~ party, scale = "free") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(limits = as.Date(c("2012-01-01", "2018-12-31")),
               date_breaks = "2 years") +
  ylim(-0.2,0.3) +
  xlab("Dato") +
  ylab("Gennemsnitlig Tweetscore (2012-2018)") +
  theme_minimal()

#GENNEMSNITSSCORE PER MÅNED I PERIODEN FOR ALLE
gns.month <- twdf %>%
  group_by(month=floor_date(created_at, "month")) %>% 
  select(created_at, balanceperord) %>% 
  mutate(mean = mean(balanceperord),
         sd = sd(balanceperord),
         ss = n(),
         SE = sd(balanceperord)/sqrt(n()))
 
#PLOTTER 
ggplot(gns.month, aes(x = month, y = mean)) +
  geom_bar(stat = "summary", fun.y = "mean", fill = "steelblue") +
  geom_errorbar(aes(ymin=mean - SE, ymax=mean + SE, width=.2)) +
  scale_x_date(limits = as.Date(c("2012-01-01", "2018-12-31")),
               date_breaks = "1 year") +
  xlab("Dato") +
  ylim(-0.05,0.1) +
  ylab("Gennemsnitlig Tweetscore pr. måned (2012-2018)") +
  theme_minimal()

#####ANALYSE: REGRESSIONSDISKONTINUITETSDESIGNET I TID####
#TJEKKER NUMMER-ÆKVIVALENT TIL RELEVANTE DATOER FOR UDSKRIVELSE AF FOLKETINGSVALG
mydates <- as.Date(c("2015-05-27"))
mydates <-  as.numeric(mydates)

mydates[1] #16582 SVARER TIL D. 27-05-2018, HVOR DER BLEV UDSKREVET VALG

#LAVER NY VARIABLE HVOR UDSKRIVELSESDATOEN ER 0 OG HHV. ANTAL DAGE EFTER OG FØR
twdf$created_at_valg15 <- twdf$created_at_num - 16582

#LAVER TREATMENT INDIKATOR DUMMY FOR VALGKAMP OG IKKE-VALGKAMP
twdf <- twdf %>%
  mutate(valgkamp = ifelse(created_at_valg15>=0, "Valgkamp", "Ikke-valgkamp")) %>% 
  group_by(created_at_valg15) %>% 
  mutate(mean = mean(balanceperord))

ttest <- twdf %>%
  filter(created_at_valg15 >= -30) %>% 
  filter(created_at_valg15 <= 22)

#KØRER T-TEST FOR FORSKEL I GENNEMSNIT FOR IKKE-VALGKAMP OG VALGKAMP
t.test(mean ~ valgkamp, data = ttest)

#PLOTTER SAMMENHÆNGEN MELLEM DATO OG BALANCEPERORD
#PLOTTER FOR ALLE
ggplot(twdf, aes(created_at_valg15, mean, colour = valgkamp, group=valgkamp)) +
  geom_point() + 
  stat_smooth(method = lm) +
  xlim(-30,22) +
  geom_vline(xintercept=0, linetype="longdash", color = "orange") +
  xlab("Antal dage før og efter udskrivelsen af Folketingsvalg 2015") +
  ylab("Gennemsnitlig Tweetscore per dag") +
  scale_y_continuous(limits = c(-0.04,0.08)) +
  scale_color_discrete(name = "Valgkamp") +
  theme_minimal()

#PLOTTER FOR HVERT PARTI
ggplot(twdf, aes(created_at_valg15, mean, colour = valgkamp, group=valgkamp)) +
  geom_point() + 
  stat_smooth(method = lm) +
  facet_wrap(~ party, scales = "free") +
  xlim(-10,10) +
  geom_vline(xintercept=0, linetype="longdash", color = "orange") +
  xlab("Antal dage før og efter udskrivelsen af Folketingsvalg 2015") +
  ylab("Gennemsnitlig Tweetscore per dag") +
  scale_y_continuous(limits = c(-0.04,0.08)) +
  scale_color_discrete(name = "Valgkamp") +
  theme_minimal()

#BENYTTER RDD-PAKKE TIL AT FORETAGE RD 
rd <- RDestimate(mean ~ created_at_valg15, data = twdf, bw = 10, kernel="rectangular", cluster = twdf$party)
summary(rd)

##ANALYSERER RDiT FOR HVERT PARTI
rd_venstre <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Venstre"),
                         bw = 10, kernel="rectangular")
summary(rd_venstre)

rd_socdem <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Socialdemokratiet"),
                         bw = 10, kernel="rectangular")
summary(rd_socdem)


rd_df <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Dansk Folkeparti"),
                         bw = 10, kernel="rectangular")
summary(rd_df)

rd_radikale <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Radikale Venstre"),
                         bw = 10, kernel="rectangular")
summary(rd_radikale)

rd_sf <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Socialistisk Folkeparti"),
                         bw = 10, kernel="rectangular")  
summary(rd_sf)

rd_alternativet <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Alternativet"),
                         bw = 10, kernel="rectangular")
summary(rd_alternativet)

rd_kons <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Konservative"),
                         bw = 10, kernel="rectangular")
summary(rd_kons)

rd_kristendemokraterne <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Kristendemokraterne"),
                         bw = 10, kernel="rectangular")
summary(rd_kristendemokraterne)

rd_liberal <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Liberal Alliance"),
                         bw = 10, kernel="rectangular")
summary(rd_liberal)

rd_enhedslisten <- RDestimate(mean ~ created_at_valg15, data =subset(twdf, party=="Venstre"),
                         bw = 10, kernel="rectangular")
summary(rd_enhedslisten)


  
