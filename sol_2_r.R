library(pacman)
pacman::p_load(httr, tidyverse, rvest, janitor)

#Gemmer url'er i objekter
grconurl <- "https://www.conservapedia.com/Essay:Greatest_Conservative_Movies"
woliburl <- "https://www.conservapedia.com/Essay:Worst_Liberal_Movies"

#Læser ind
grcon <- read_html(grconurl)
wolib <- read_html(woliburl)

#Scraper tabel for "Greatest Conservative Movies" og gør til data.frame
cons_tables <- html_nodes(grcon, "table")
cons_dat <- rbind(html_table(cons_tables[[1]], fill = TRUE),
                  html_table(cons_tables[[2]], fill = TRUE),
                  html_table(cons_tables[[3]], fill = TRUE),
                  html_table(cons_tables[[4]], fill = TRUE),
                  html_table(cons_tables[[5]], fill = TRUE)) %>%
  clean_names()

# Tilføjer kolonne til variablen 'cons_dat' og giver værdien 0 til alle konservative film (skal bruges senere)
cons_dat$political <- rep(0,nrow(cons_dat))

#Scraper liberale film tabeller og binder til data.frame
lib_tables <- html_nodes(wolib, "table")

lib_dat <- rbind(html_table(lib_tables[[1]], fill = TRUE),
                  html_table(lib_tables[[2]], fill = TRUE),
                  html_table(lib_tables[[3]], fill = TRUE),
                  html_table(lib_tables[[4]], fill = TRUE),
                  html_table(lib_tables[[5]], fill = TRUE),
                  html_table(lib_tables[[6]], fill = TRUE)) %>%
  clean_names()

#Tilføjer kolonne og giver alle Liberale Film værdien 1 (skal bruges senere)
lib_dat$political <- rep(1, nrow(lib_dat))


#Appender de to data.frames, dvs. sammensætter på rækker
film <- rbind(lib_dat, cons_dat)


#Fjerner forstyrrende elementer i gross_domestic kolonne i film data.frame
film$box <- film$gross_domestic %>% 
  str_replace(
    pattern = ',',
    replacement = ''
) %>% 
  str_replace(
    pattern = '\\$',
    replacement = ''
  ) %>% 
  str_replace(
    pattern = '\\[.*\\]',
    replacement = ''
  ) %>% 
  str_replace(
    pattern = '\\(.*\\)',
    replacement = ''
  )


#Konverterer kolonne gross_domestic til numerisk variabel
film$boxoffice <- as.numeric(film$box)


#Computerer gennemsnit for gross_domestic for hhv. konservative og liberale film
film %>% 
  mutate(millions = boxoffice/1000000) %>% 
  group_by(political) %>%
  summarize(mean_millions = mean(millions, na.rm = TRUE))

# T-test
t.test(film$boxoffice ~ film$political) # p = 0.0942, diff. er ikke signifikant indenfor konventionel measure

#Kigger på top 5 film
film %>% 
  group_by(political) %>%  
  top_n(5, boxoffice)
