library(pacman)
p_load(rio, tidyverse)

russiantrolls <- import("https://github.com/fivethirtyeight/russian-troll-tweets/raw/master/IRAhandle_tweets_1.csv")

str(russiantrolls)


#OPGAVE 1
class(russiantrolls$language) #language variabel er character
table(russiantrolls$language)

russiantrolls %>%
  count(language) %>% 
  arrange(desc(n))  # 1. English, 2. Russian

#OPGAVE 2
russiantrolls %>% 
  group_by(region) %>% 
  summarize(sumfo = sum(followers)) %>% 
  arrange(desc(sumfo))  #1. United States  =   502538351

#OPGAVE 3
russiantrolls %>% 
  filter(retweet==1) %>% 
  count(language) %>% 
  arrange(desc(n)) #1. English = 105982

#OPGAVE 4
russiantrolls %>% 
  summarize(donald = sum(str_count(content, pattern = "Trump"))) #30959 gange nævnt
 
russiantrolls %>% 
  summarize(hillary = sum(str_count(content, pattern = "Clinton"))) #4801 gange nævnt
                         