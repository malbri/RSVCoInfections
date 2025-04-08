##Input to SBI model optimization

rm(list = ls())

#Load packages
library(tidyverse)
library(dplyr)
library(stringr)

#Load SEIR solution
sol <- readRDS(file = "Output/rsvModelSol2.rds")

#Calculate mean of the n runs
sol <- sol %>% group_by(time,state) %>% summarise(meanVal = mean(n),
                                                  minVal = min(n),
                                                  maxVal = max(n))

#Extract I2
Iresultmean <- sol %>% 
  filter(str_detect(state,"I2")) %>% ungroup() %>% select(time,state,meanVal) %>% 
  pivot_wider(names_from = state, values_from = meanVal) %>% select(-time) %>% as.matrix()


#Save RDS files
saveRDS(Iresultmean, file = "Output/Iresultmean.rds")

rm(list = ls())

##Input to SBI model run
#Load SEIR solution
sol <- readRDS(file = "Output/rsvModelSol.rds")

#extract I
IresultN <- sol %>% filter(str_detect(state,"I")) %>% mutate(ageGroup = str_sub(state, -1)) %>% 
  select(-state)

#Prepare list for adding the n runs
Iresultlist <- list()

#Add the n runs to a list
for (run in unique(sol$node)){
  NresultFromRun <- IresultN %>% filter(node == run) %>% select(time, ageGroup, n) %>% 
    pivot_wider(names_from = ageGroup, values_from = n) %>% select(-time) %>% as.matrix()
    Iresultlist[[run]] <- NresultFromRun
  }

#Save RDS files
saveRDS(Iresultlist, file = "Output/Iresultlist.rds")



