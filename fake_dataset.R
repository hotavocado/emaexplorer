library(tidyverse)
library(stringr)

#creating fake dataset for VISTA app
starwars <- starwars



#create number variables for films, vehicles, starships
listvars <- names(starwars[11:13])

starwars <- starwars %>% mutate_at(listvars, funs(number = sapply(., length)))


#brute forcing right now, find out how to program later

#making dummy variables from films list column
#starwars <- starwars %>% mutate(`Revenge of the Sith` = sapply(films, function (x) ifelse(movies[[1]] %in% x, 'yes', 'no')),
#                                `Return of the Jedi` = sapply(films, function (x) ifelse(movies[[2]] %in% x, 'yes', 'no')),
#                                `The Empire Strikes Back` = sapply(films, function (x) ifelse(movies[[3]] %in% x, 'yes', 'no')),
#                                `A New Hope` = sapply(films, function (x) ifelse(movies[[4]] %in% x, 'yes', 'no')),
#                                `The Force Awakens` = sapply(films, function (x) ifelse(movies[[5]] %in% x, 'yes', 'no')),
 #                               `Attack of the Clones` = sapply(films, function (x) ifelse(movies[[6]] %in% x, 'yes', 'no')),
  #                              `The Phantom Menance` = sapply(films, function (x) ifelse(movies[[7]] %in% x, 'yes', 'no')))
                            

#function to create dummy columns

dummyships <- function (data, ships, column) {
  
  column = enquo(column)
  
  test <- data %>% dplyr::mutate(!!ships := sapply(!!column, function (x) ifelse(!!ships %in% x, 1, 0)))
  
  print(paste0("column ", tail(names(test), n=1), " created!"))
  
  return(test)
  
}


#make movies list
movies <- unique(unlist(starwars[['films']]))

#make columns
for (i in movies) {
  
  starwars <- dummyships(starwars, i, films)
  
}

#vehicles list
vehicles_list <-  unique(unlist(starwars[['vehicles']]))

#make columns
for (i in vehicles_list) {
  
  starwars <- dummyships(starwars, i, vehicles)
  
}

#starships list
starships_list <- unique(unlist(starwars[['starships']]))

#make columns
for (i in starships_list) {
  
  starwars <- dummyships(starwars, i, starships)
  
}


#Add some preambles to these dummy variables

starwars <- starwars %>% rename_at(movies, ~paste0("Starred in_", .))

starwars <- starwars %>% rename_at(vehicles_list, ~paste0("Rode in_", .))

starwars <- starwars %>% rename_at(starships_list, ~paste0("Flew in_", .))


#flatten the list variables
starwars <- starwars %>% mutate_at(listvars, ~sapply(., toString))


#change all spaces in column names to underscore 

corrected_names <- str_replace_all(names(starwars), " ", "_")

corrected_names <- str_replace_all(corrected_names, "-", "_")

colnames(starwars) <- corrected_names

#Load in EMA data

EMA <- read.csv('EMAApp100//data//EMAPalm_1_25_18.csv')


#select variables to include in dataset

EMA <- EMA %>% select(ID, day, signal, Morning, Noon, Afternoon, Evening, time, Alcohol, caffeine, hrsleep, Energy, sad, anxious, active, irrit, hungry, globstr,
                      meal, chores, friends, home, working, computer, sexacti, qlsleep, sports)


#select 87 random subjects

EMAsubs <- unique(EMA$ID)
mysubs <- sample(EMAsubs, 87, replace = F)

EMA <- filter(EMA, ID %in% mysubs)

#replace subject IDs with starwars names
EMA$ID <- rep(starwars$name, each = 56)

#rename name column in star wars dataset
starwars <- starwars %>% rename(ID = name)


#rename variables in EMA dataset

#[1] "ID"        "day"       "signal"    "Morning"   "Noon"      "Afternoon" "Evening"   "time"      "Alcohol"   "caffeine"  "hrsleep"   "Energy"   
#[13] "sad"       "anxious"   "active"    "irrit"     "hungry"    "globstr"   "meal"      "chores"    "friends"   "home"      "working"   "computer" 
#[25] "sexacti"   "qlsleep"   "sports"   


starwars_rename <- c("ID", "day", "signal", "Morning", "Noon", "Afternoon", "Evening", "time", "used_bacta_extract", "used_the_force", "hours_slept", 
                     "blood_midichlorian_content", "sadness_level", "anxiety_level", "activeness_level", "irritability_level", "hunger_level", "stress_level", "had_meal",
                     "won_a_pod_race", "with_friends", "location_starship", "location_cantina", "played_holochess", "fought_in_duel", "daily_force_sensitivity", "used_lightsaber")


EMA <- EMA %>% rename_at(vars(names(EMA)), ~ starwars_rename)


#transform variables
EMA <- EMA %>%  mutate(blood_midichlorian_content = 1/jitter(blood_midichlorian_content, amount = 0.5),
                       sadness_level = jitter(sadness_level, amount = runif(1)),
                       anxiety_level = jitter(anxiety_level, amount = runif(1)),
                       activeness_level = jitter(activeness_level, amount = runif(1)),
                       irritability_level = jitter(irritability_level, amount = runif(1)),
                       hunger_level = jitter(hunger_level, amount = runif(1)),
                       stress_level = jitter(stress_level, amount = runif(1)))


#create max values data for compliance

max <- as_data_frame(matrix(ncol = 27, nrow = 1))

colnames(max) <- names(EMA)

max[1,] = 56

max$qlsleep = 14



                       
                       
                       

#export datasets
#setwd('C:\\Users\\Mike\\Documents\\RProjects\\emaexplorer')

write_csv(EMA, 'EMAApp100/data/starwars_EMA.csv')
write_csv(starwars, 'EMAApp100/data/starwars_demo.csv')
write_csv(max, 'EMAApp100/data/starwars_max.csv')
