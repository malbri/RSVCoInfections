rm(list = ls())

#Load packages
library(tidyverse)
library(skimr)
library(stringr)
library(readxl)

#Load the SSI case data
rsv <-read_excel("Data/case_data.xlsx") 

#Keep only data for "all" regions (no need for region specifics)
rsv <- rsv %>% filter(Region == "Alle") %>% select(-c(Region,Regionskode))

#Keep only data for "both" genders (no need for gender specifics)
rsv <- rsv %>% filter(Køn == "Alle") %>% select(-c(Køn))

#Remove the last 4 columns since they are not needed
rsv <- rsv[,2:9]

#Rename columns to English
rsv <- rsv %>% rename(season = Sæson, week = Uge, ageGroup = Aldersgruppe,
                                  pop = `Antal borgere`, tests = `Antal testede personer`, confirmed = `Antal Bekræftede tilfælde`,
                                  hospitalizations = `Antal nye indlæggelser`, deaths = `Antal døde`)


#Remove population
rsv <- rsv %>% select(-pop)

#Sort by week
rsv <- rsv %>% arrange(week) %>% ungroup()

#Add start date of week to data
dates <- seq(as.Date("2015-09-28"), as.Date("2024-10-15"), by = "7 days")
startDate <- rep(dates, each = 12)

rsv$startDate <- startDate

#Save data
saveRDS(rsv, file = "Data/cases_clean.rds")


#Remove the totals
cases <- rsv %>% filter(ageGroup != "Alle")

cases <- cases %>% mutate(ageGroup = str_trim(ageGroup)) %>%  # Trim leading and trailing spaces
  mutate(ageGroup = case_when(
    ageGroup %in% c("0-05","06-011") ~ "1",
    ageGroup %in% c("1","2","3-5") ~ "2",
    ageGroup %in% c("6-14") ~ "3",
    ageGroup %in% c("15-44") ~ "4",
    ageGroup %in% c("45-64") ~ "5",
    ageGroup %in% c("65-74","75-84", "85+") ~ "6"
))

cases <- cases %>% group_by(season, week, ageGroup, startDate) %>% summarise(tests = sum(tests),
                                                                             confirmed = sum(confirmed),
                                                                             hospitalizations = sum(hospitalizations),
                                                                             deaths = sum(deaths)) %>% ungroup()

#Save data
saveRDS(cases, file = "Data/casesValidated.rds")

#TEST #345
cases <- cases %>% mutate(ageGroup = str_trim(ageGroup)) %>%  # Trim leading and trailing spaces
  mutate(ageGroup = case_when(
    ageGroup %in% c("0-05","06-011") ~ "1",
    ageGroup %in% c("1","2","3-5") ~ "2",
    ageGroup %in% c("6-14", "15-44", "45-64" ) ~ "345",
    ageGroup %in% c("65-74","75-84", "85+") ~ "6"
  ))

cases <- cases %>% group_by(season, week, ageGroup, startDate) %>% summarise(tests = sum(tests),
                                                                             confirmed = sum(confirmed),
                                                                             hospitalizations = sum(hospitalizations),
                                                                             deaths = sum(deaths)) %>% ungroup()

#Save data
saveRDS(cases, file = "Data/casesValidated_345.rds")

