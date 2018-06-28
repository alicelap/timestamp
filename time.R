library(tidyverse)
library(lubridate)
setwd("/Users/Alice/University of Warwick/Rathelot, Roland - CB_IL/Build_data/Step1/output/")
db <- readRDS("apps_IL_withtimestamp.rds")

# Oddities 

# People applying more than once to a given vacancy
# proposition de filtre (similaire à Faberman et Kudlyak)
# enlever les candidature à moins d'une semaine d'intervalle 
# garder ceux à plus d'une semaine d'intervalle

same_vacancy0 <- db %>% 
  select(-c("first_application", "last_application")) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%  # removes unique application from one user to a vacancy
  mutate(
    first_application = min(applicationtime), # first application to this very vacancy (≠ before)
    laps_between_application = (applicationtime - first_application) / dweeks(1), 
    keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
    ) %>%
  filter(keep==1) # removes all applications within the week of the first application 

garbage0 <- db %>% 
  select(-c("first_application", "last_application")) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%  
  mutate(
    first_application = min(applicationtime),
    laps_between_application = (applicationtime - first_application) / dweeks(1), 
    keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
    ) %>%
  filter(keep==0) %>% # 19,241 observations
  select(userid,jobid,applicationtime) # uniquely identifies the row 

same_vacancy2 <- same_vacancy0 %>% 
  filter(laps_between_application > 0) %>% # on ne garde que les observations susceptibles de poser problème
  group_by(userid,jobid) %>% 
  mutate(count=n()) %>%
  filter(count>1) %>% # 7,157 observations 
  mutate(
    first_application = min(applicationtime),
    laps_between_application = (applicationtime - first_application) / dweeks(1),
    keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
    ) %>%
  filter(keep==1) # 5,13 observations 

garbage2 <- same_vacancy0 %>% 
  filter(laps_between_application > 0) %>% 
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(
    first_application = min(applicationtime),
    laps_between_application = (applicationtime - first_application) / dweeks(1),
    keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
    ) %>%
  filter(keep==0) %>% # 2,019 observations
  select(userid,jobid,applicationtime) 

same_vacancy3 <- same_vacancy2 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 943 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 645 observations 

garbage3 <- same_vacancy2 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) %>% # 298 observations 
  select(userid,jobid,applicationtime) 

same_vacancy4 <- same_vacancy3 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 177 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 131 observations 

garbage4 <- same_vacancy3 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) # 46 observations 

same_vacancy5 <- same_vacancy4 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 32 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 23 observations 

garbage5 <- same_vacancy4 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) # 9 observations 

same_vacancy6 <- same_vacancy5 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 9 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 6 observations 

garbage6 <- same_vacancy5 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) # 3 observations 

same_vacancy7 <- same_vacancy6 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 2 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 1 observations 

garbage6 <- same_vacancy6 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) # 1 observations 

same_vacancy8 <- same_vacancy7 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 0 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 0 observations 

garbage6 <- same_vacancy6 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) # 0 observations 

# Idée :
# automatiser cette boucle et l'arrêter lorsque nrow(garbagek)==0
# puis enlever les observations de db si elles sont dans (garbage,...garbagek)
# Illinois : 21 614 observations

# Number of applications per userid
db %>% 
  group_by(userid) %>%
  count() %>%
  ggplot(db, mapping=aes(n)) +
    geom_histogram() + 
  coord_cartesian(ylim = c(0, 30))

extreme <- db %>% 
  group_by(userid) %>%
  filter(nba > 500)

user_116954 <- db %>% filter(userid == 116954 & month(applicationtime)==6 & day(applicationtime)==18) %>% group_by(hour(applicationtime)) %>% count()
# restriction to a single userid, on a given day (June 18th) : sent in 389 applications 

extreme2 <- db %>% 
  select(-c("jobid","first_application","last_application")) %>%
  group_by(userid,month(applicationtime),day(applicationtime)) %>% 
  mutate(nba_on_a_given_day = n()) %>%
  ungroup %>%
  group_by(userid) %>%
  filter(nba_on_a_given_day > 200) %>%
  distinct(userid) # 69 userid sent more than 200 applications per day 