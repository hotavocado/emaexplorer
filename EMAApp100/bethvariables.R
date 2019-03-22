#BETH VARIABLES

library(tidyverse)

setwd("C:\\Users\\Mike\\Documents\\RProjects\\emaexplorer\\EMAApp100")
NIMHMerged <- read.csv("data/EMA_Master_3_1_19.csv", na.strings = c("NA", "NaN", ""), stringsAsFactors = F) 


#context variables

#PD_alone: alone (1=yes, 0=no)
#PD_familiar: if observation is either PD_fampart = 1, PD_friends =1, PD_pet =1
#PD_other: if observation is PD_strange =1, PD_withothr =1, PD=collcl =1

#location variables
#Home: yes =1, 0= no
#Location_familiar =  if observation is either PD_workcl =1, PD_vehic =1, PD_homerf =1
#Location_other =  if observation is 1 for these PD_hospcl, PD_othrin, PD_othrout, PD_gymstad, PD_othrout, PD_restbar, PD_pubbldg, PD_storshop


NIMHMerged <- NIMHMerged %>% 
              mutate(Beth_alone = as.character(PD_alone),
                     
                     Beth_familiar = as.character(ifelse(is.na(PD_fampart) & is.na(PD_friends) &  is.na(PD_pet), NA,
                                            ifelse(PD_fampart == 1 |PD_friends == 1 | PD_pet == 1, 1, 0))),
                     
                     Beth_other = as.character(ifelse(is.na(PD_strange) & is.na(PD_withothr) &  is.na(PD_collcl), NA,
                                         ifelse(PD_strange == 1 |PD_withothr == 1 | PD_collcl == 1, 1, 0))),
                     
                     Beth_context = as.character(ifelse(Beth_alone == 1, "alone", 
                                                               ifelse(Beth_familiar == 1, "familiar",
                                                                      ifelse(Beth_other == 1, "other", NA)))),
                     
                     Beth_location_home = as.character(PD_home),
                     
                     Beth_location_familiar = as.character(ifelse(is.na(PD_workcl) & is.na(PD_vehic) &  is.na(PD_homerf), NA,
                                                         ifelse(PD_workcl == 1 |PD_vehic == 1 | PD_homerf == 1, 1, 0))),
                     
                     Beth_location_other = as.character(ifelse(is.na(PD_hospcl) & is.na(PD_othrin) &  is.na(PD_othrout) & is.na(PD_gymstad)  &  is.na(PD_gymstad)  &  is.na( PD_othrout)  &  is.na(PD_restbar)  &  is.na(PD_pubbldg) &  is.na(PD_storshop),  NA,
                                                      ifelse(PD_hospcl == 1 |PD_othrin == 1 | PD_othrout == 1 |PD_gymstad == 1 | PD_gymstad == 1 | PD_othrout == 1 | PD_restbar == 1 | PD_pubbldg == 1 | PD_storshop == 1, 1, 0))),
                    
                     Beth_location = as.character(ifelse(Beth_location_home == 1, "home", 
                                                        ifelse(Beth_location_familiar == 1, "familiar",
                                                               ifelse(Beth_location_other == 1, "other", NA))))
              )


write.csv(NIMHMerged, "data/EMA_Master_3_1_19.csv", row.names = F)


