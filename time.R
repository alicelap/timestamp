library(tidyverse)
library(lubridate)
library(here)

apps_IL <- readRDS(here("Build_data","Step1","output","apps_IL_withtimestamp.rds"))
jobs_IL <- readRDS(here("Build_data","Step1","output","jobs_IL.rds"))
users_IL <- readRDS(here("Build_data","Step1","output","users_IL.rds"))

# Préparation de la base

apps_IL %>% group_by(userid) # initially : 30,757 userid

db <- apps_IL %>% 
  # Remove missing ids
  filter(!is.na(jobid),!is.na(userid)) %>% # removes 704 observations/userid
  # Format application time as date
  mutate(applicationtime = ymd_hms(applicationtime)) %>%
  # Remove duplicated in ids and app time
  distinct(userid,jobid,applicationtime) %>% # removes 43 127 observations but no userid
  # Merge with jobs and users datasets
  left_join(jobs_IL %>% filter(!is.na(statejob)),by="jobid") %>% 
  left_join(users_IL %>% select(-"currentlyemployed"), by="userid") %>%
  # Create simplified degree and occupation variables
  mutate(
    degreetype=substring(degreetype,1,1), 
    resumeonet1 = substring(resumeonet1,1,2),
    onet = substring(onet, 1,2)) %>%
  # Within each userid, first app, last app, total nb of app, nb of unique occupation applied to
  arrange(userid,applicationtime) %>% 
  group_by(userid) %>%
  mutate(
    delta_apptime = applicationtime-lag(applicationtime),
    med_delta_apptime = median(delta_apptime, na.rm = TRUE),
    first_application = min(applicationtime),
    last_application = max(applicationtime),
    distinct_ONET = length(unique(onet)),
    nba = n() # total number of applications 
    ) %>% 
  ungroup() %>% 
  mutate(
    # time between first and last apps
    time_duration = difftime(last_application,first_application,units="weeks"),
    # average number of applications per week 
    nba_per_week = ifelse(1 > time_duration, nba, nba/as.numeric(time_duration))
    )

db %>% glimpse

## First criterion: median time between apps below 10 seconds

## Diagnosis
db %>% select(userid, nba, med_delta_apptime) %>% 
  distinct() %>% 
  filter(med_delta_apptime < 60) %>% 
  ggplot(aes(x=med_delta_apptime)) + geom_density()

## How many do we lose? 
# 3759/30053 userid
db %>% select(userid, nba, med_delta_apptime) %>% 
  distinct() %>% count(med_delta_apptime < 10)
# 354k/895k apps
db %>% count(med_delta_apptime < 10)

## Treatment
db <- db %>% filter(med_delta_apptime > 10)


## Second criterion: nb of apps per week

## Diagnosis  
db %>% ggplot(aes(x=nba_per_week)) + geom_density()

## How many do we lose? 
# 85/23400 userid
db %>% select(userid, nba, nba_per_week) %>% 
  distinct() %>% count(nba_per_week > 50)
# 24k/538k apps
db %>% count(nba_per_week > 50)

## Treatment
db <- db %>% filter(nba_per_week < 50)


## Third criterion: max of nb of apps per day

## Diagnosis  
db %>% ggplot(aes(x=nba_per_week)) + geom_density()

db <- db %>% 
  group_by(userid, month(applicationtime), day(applicationtime)) %>%
  mutate(nba_day = n()) %>%
  group_by(userid) %>%
  mutate(nba_day = max(nba_day)) %>% 
  ungroup

db %>% ggplot(aes(x=nba_day)) + geom_density()

## How many do we lose? 
# 149/23300 userid
db %>% select(userid, nba, nba_day) %>% 
  distinct() %>% count(nba_day > 50)
# 32k/514k apps
db %>% count(nba_day > 50)

## Treatment
db <- db %>% filter(nba_day < 50)

## Diagnosis  
db %>% group_by(userid,jobid) %>% 
  summarise(nba_samejob = n()) %>% 
  ggplot(aes(x=nba_samejob)) + geom_histogram(binwidth = 1)




db %>% group_by(userid) %>%
  summarise(n = n(), nba = median(nba)) %>% glimpse

# Number of applications per userid
db %>% select(userid, nba) %>% distinct() %>% 
  count(nba) %>% 
  ggplot(aes(x=nba,y=n)) + geom_point() + 
  scale_x_log10() + scale_y_log10() + 
  labs(x="Number of applications",y="Number of users")


##################################
###  Statistiques descriptives ###
##################################


###   Durée  ###
# durée restée sur le site

## Number of userid by time spent on the site
db %>% select(userid, time_duration) %>% distinct() %>% 
  ggplot(aes(time_duration)) + geom_histogram()

## Same, weighted by the number of app per user
## As expected, users with more apps stay longer
db %>% ggplot(aes(time_duration)) + geom_histogram()



  
  
###   Evolution des critères de recherche  ###
# stats desc sur les caractéristiques des jobs (+ distance)

# ONET 
db <- db %>% mutate(same_ONET = ifelse(resumeonet1==onet,1,0)) 

# statique : 

# part des jobid dans le même ONET que resume 
db %>% 
  filter(!(resumeonet1 %in% c(97,99))) %>%
  select(userid, jobid,onet,resumeonet1,distinct_ONET,same_ONET) %>%
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  ggplot(db, mapping=aes(x=prop_inside)) +
  geom_histogram()  

# expliquer les 0% dans prop_inside 
db %>% 
  mutate(prop_inside = mean(same_ONET)*100) %>%
  filter(prop_inside==0) %>%
  ggplot(db, mapping=aes(x=resumeonet1)) +
  geom_bar()    
# 99 et 97 ? est-ce que c'est une sorte de NA ? 

db %>% 
  mutate(prop_inside = mean(same_ONET)*100) %>%
  filter(prop_inside==0) %>%
  ggplot(db, mapping=aes(x=nba, y=..prop..,group=1)) +
  geom_bar() 

# mean of prop_inside
db %>% 
  group_by(userid) %>%
  filter(!(resumeonet1 %in% c(97,99))) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  summarise(mean = mean(prop_inside), n = n()) %>% 
  ungroup() %>%     
  summarise(mean1=mean(mean, na.rm=T))

# évolution prop_inside avec le temps passé au chômage
db %>% 
  filter(!(resumeonet1 %in% c(97,99))) %>% # brut
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  group_by(time_duration,prop_inside) %>% 
  distinct(userid) %>%
  group_by(time_duration) %>%
  summarise(mean = mean(prop_inside, na.rm=T)) %>% 
  ggplot(db, mapping=aes(x=time_duration,y=mean)) + # time_duration = total time duration 
  geom_smooth()  

db %>% 
  filter(nba > 7 & !(resumeonet1 %in% c(97,99))) %>% # filte pour "stabiliser" les pourcentages, surtout au début.. 
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  group_by(time_duration,prop_inside) %>% 
  distinct(userid) %>%
  group_by(time_duration) %>%
  summarise(mean = mean(prop_inside, na.rm=T)) %>% 
  ggplot(db, mapping=aes(x=time_duration,y=mean)) + # time_duration = total time duration 
  geom_smooth()  


# évolution du nombre de ONET différents auxquels les utilisateurs postulents selon la durée au chômage
db %>%
  filter(!(resumeonet1 %in% c(97,99))) %>%
  group_by(time_duration,distinct_ONET) %>% 
  distinct(userid) %>% # ne garde qu'une ligne par userid
  group_by(time_duration) %>%    
  summarise(mean = mean(distinct_ONET, na.rm=T)) %>%
  ggplot(db, mapping=aes(x=time_duration,y=mean)) +
  geom_smooth()  
# corrélation positive temps au chômage et nombre de ONE distincts

#  lien entre prop inside & nba (et resumeonet1 ou degreetype)..   
db %>% 
  filter(!(resumeonet1 %in% c(97,99))) %>%
  select(userid,same_ONET,nba, resumeonet1) %>%
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  ggplot(db, mapping = aes(x = nba, y = prop_inside, color=resumeonet1)) +
  geom_point(position="jitter")

db %>% filter(nba > 5) %>%
  filter(!(resumeonet1 %in% c(97,99))) %>%
  select(userid,same_ONET,nba, degreetype) %>% # degreetype ou resumeonet1 : résultats similaires
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  group_by(userid,prop_inside,degreetype, nba) %>%
  distinct(userid) %>%
  ggplot(db, mapping = aes(x = nba, y = prop_inside, color=degreetype)) + 
  geom_point(position="jitter")




# ceux qui ne candidate jamais dans la même ONET 
db %>% 
  select(userid, jobid,onet,resumeonet1,distinct_ONET,same_ONET) %>%
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  filter(prop_inside==0) %>%
  ggplot(db, mapping=aes(x=distinct_ONET, y = ..prop.., group = 1)) + # nombre de ONET différents auxquels ils postulent 
  geom_bar()  
# 65% < 3 distinct ONET
  
# est-ce que cela correspond à des ONET particuliers ? 
db %>% 
  select(userid, jobid,onet,resumeonet1,distinct_ONET,same_ONET) %>%
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  filter(prop_inside==0) %>%
  ggplot(db, mapping=aes(x=resumeonet1,y=onet)) + # ONET auxquels ils postulent 
  geom_point(position="jitter")  

# lien avec le niveau de diplôme 
db %>% 
  group_by(userid) %>%
  mutate(prop_inside = mean(same_ONET)*100) %>%
  filter(prop_inside==0) %>%
  ggplot(db, mapping=aes(x=degreetype, y = ..prop.., group = 1)) + 
  geom_bar() + 
  labs(title="Distribution of degree type among those who do not apply to the same ONET")

db %>% 
  ggplot(db, mapping=aes(x=degreetype, y = ..prop.., group = 1)) + 
  geom_bar()  + 
  labs(title="Distribution of degree-type")














# Numerous applications to a given vacancy by the same user

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
  filter(keep==0) %>% # 5,960 observations
  select(userid,jobid,applicationtime) # uniquely identifies the row 

same_vacancy1 <- same_vacancy0 %>% 
  filter(laps_between_application > 0) %>% # on ne garde que les observations susceptibles de poser problème
  group_by(userid,jobid) %>% 
  mutate(count=n()) %>%
  filter(count>1) %>% # 2,378 observations 
  mutate(
    first_application = min(applicationtime),
    laps_between_application = (applicationtime - first_application) / dweeks(1),
    keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
    ) %>%
  filter(keep==1) # 1,813 observations 

garbage1 <- same_vacancy0 %>% 
  filter(laps_between_application > 0) %>% 
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(
    first_application = min(applicationtime),
    laps_between_application = (applicationtime - first_application) / dweeks(1),
    keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
    ) %>%
  filter(keep==0) %>% # 565 observations
  select(userid,jobid,applicationtime) 

same_vacancy2 <- same_vacancy1 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 325 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 253 observations 

garbage2 <- same_vacancy1 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) %>% # 72 observations 
  select(userid,jobid,applicationtime) 

same_vacancy3 <- same_vacancy2 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 70 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 53 observations 

garbage3 <- same_vacancy2 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) %>% # 17 observations 
  select(userid,jobid,applicationtime) 

same_vacancy4 <- same_vacancy3 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 23 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 18 observations 

garbage4 <- same_vacancy3 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% 
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) %>% # 5 observations 
  select(userid,jobid,applicationtime) 

same_vacancy5 <- same_vacancy4 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 8 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 > laps_between_application & laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 6 observations 

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
  filter(keep==0) %>% # 2 observations 
  select(userid,jobid,applicationtime) 

same_vacancy6 <- same_vacancy5 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 4 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 4 observations 

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
  filter(keep==0) %>% # 0 observations
  select(userid,jobid,applicationtime) 

same_vacancy7 <- same_vacancy6 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>% # 3 observations
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==1) # 2 observations 

garbage7 <- same_vacancy6 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
         ) %>%
  filter(keep==0) %>% # 1 observation 
  select(userid,jobid,applicationtime) 

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
  filter(keep==1) # 0 observation 

garbage8 <- same_vacancy7 %>%
  filter(laps_between_application > 0) %>%
  group_by(userid,jobid) %>%
  mutate(count=n()) %>%
  filter(count>1) %>%
  mutate(applicationtime = ymd(applicationtime),
         first_application = min(applicationtime),
         laps_between_application = (applicationtime - first_application) / dweeks(1),
         keep = ifelse( 1 >laps_between_application &  laps_between_application > 0,0,1)
  ) %>%
  filter(keep==0) %>% # 0 observation
  select(userid,jobid,applicationtime) 

# Idée :
# automatiser cette boucle et l'arrêter lorsque nrow(garbagek)==0
# puis enlever les observations de db si elles sont dans (garbage,...garbagek)
#  7,982 observations
nrow(garbage0) + nrow(garbage1) + nrow(garbage2) + nrow(garbage3) + nrow(garbage4) + nrow(garbage5) + nrow(garbage6) + nrow(garbage7) + nrow(garbage8)    

# Début de reprise 
db %>% group_by(userid,jobid) %>% 
  summarise(nba_samejob = n()) %>% ungroup() %>% count(nba_samejob > 3)


for (){
  db <- db %>% mutate() %>% filter()
}
