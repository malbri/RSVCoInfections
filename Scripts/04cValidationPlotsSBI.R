setwd("C:/Users/malbri/OneDrive - Danmarks Tekniske Universitet/Dokumenter/4_RSV/RSV_project_2")
rm(list = ls())

#Libraries
library(tidyverse)
library(patchwork)
library(lubridate)

#Colors
colors <- c("#D95F02", "#7570B3", "#E7298A", "#A76854", "#66A61E", "#E6AB02", "#45310C","#666666")

optimSBI <- readRDS(file = "Output/SBIOptim.rds")

#Load SEIR solution
sol <- readRDS(file = "Output/SBIsol.rds")

#Extract age group information from sol
sol <- sol %>% mutate(ageGroup = str_extract(state, "[0-6]$"),
               state = str_remove(state, "[0-6]$"),
               state = str_replace(state, pattern = "\\.",replacement = ""))

#Load augmented file of influenza cases
cases <- readRDS(file = "Data/casesValidated.rds")

#Calculate cumulative confirmed cases, to compare to R state

cases <- cases %>% filter(startDate >= as.Date("2023-09-18"), startDate <= as.Date("2024-05-18"))%>%
  group_by(ageGroup) %>% mutate(cumHosp = cumsum(hospitalizations),
                                cumDeath = cumsum(deaths)) %>% ungroup() 

# Compare hospitalizations in the model with data for the new age groups

# Age Group 1
hosp1 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup %in% c("1")) %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp1Week <- sapply(seq(1, length(hosp1), by = 7), function(i) sum(hosp1[i:(i+6)]))

# Age Group 2
hosp2 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup %in% c("2")) %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp2Week <- sapply(seq(1, length(hosp2), by = 7), function(i) sum(hosp2[i:(i+6)]))

# Age Group 3
hosp3 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup %in% c("3")) %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp3Week <- sapply(seq(1, length(hosp3), by = 7), function(i) sum(hosp3[i:(i+6)]))

# Age Group 4
hosp4 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup %in% c("4")) %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp4Week <- sapply(seq(1, length(hosp4), by = 7), function(i) sum(hosp4[i:(i+6)]))

# Age Group 5
hosp5 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup == "5") %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp5Week <- sapply(seq(1, length(hosp5), by = 7), function(i) sum(hosp5[i:(i+6)]))

# Age Group 6
hosp6 <- sol %>% 
  filter(state %in% c("HRSVCumu", "HMpCumu","HSpCumu", "HSaCumu"), ageGroup == "6") %>% 
  group_by(time) %>% 
  summarise(n = sum(n)) %>% 
  mutate(dailyHosp = n - lag(n)) %>% 
  select(dailyHosp) %>% pull()
hosp6Week <- sapply(seq(1, length(hosp6), by = 7), function(i) sum(hosp6[i:(i+6)]))


# Weekly hospitalization data from actual cases
hospData <- cases %>% 
  select(startDate, ageGroup, hospitalizations) %>% 
  pivot_wider(id_cols = startDate, names_from = ageGroup, values_from = hospitalizations) %>% 
  select(!startDate) %>% 
  mutate(from = "data", time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "7 days"))

# Aggregate columns in hospData to match the new age groups in hospComb
hospData <- hospData %>%
  mutate(
    "1" = `1`, 
    "2" = `2`,
    "3" = `3`,
    "4" = `4`,
    "5" = `5`,
    "6" = `6`,
   ) %>%
  # Keep only the necessary columns and add `from` and `time` columns
  select(time, "1", "2", "3", "4", "5", "6") %>%
  mutate(from = "data")


# Combine model estimates and actual data for comparison
hospComb <- tibble(
  time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "7 days"),
  "1" = hosp1Week,
  "2" = hosp2Week,
  "3" = hosp3Week,
  "4" = hosp4Week,
  "5" = hosp5Week,
  "6" = hosp6Week,
  from = "est"
)


#Adding hospital data
hospComb <- hospComb %>% add_row(hospData)

hospComb <- hospComb %>% pivot_longer(cols = !c(time,from), names_to = "ageGroup", values_to = "n")

hospCompPlot <- hospComb %>%
  ggplot(aes(x = time, y = n, col = from, group = from)) +
  geom_line() +
  geom_point(size = 0.5) +
  scale_color_manual(values = colors[c(3, 5)]) +
  facet_wrap(~ageGroup, labeller = as_labeller(c(
    "1" = "<1 years",
    "2" = "1-5 years", 
    "3" = "6-14 years",
    "4" = "15-44 years",
    "5" = "45-64 years",
    "6" = "65+ years"
  ))) +
  theme_bw(base_size = 12) +
  scale_x_date(date_labels = "%d %b") +
  labs(title = "A. Weekly New Hospitalizations", x = "", y = "", col = "") +
  theme(legend.position = "none")

print(hospCompPlot)
ggsave(hospCompPlot,filename = "Plots/hospComp_2.png", height = 3.16, width = 7)

# Compute weekly deaths for each new age group
deaths1 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup %in% c("1")) %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths1Week <- sapply(seq(1, length(deaths1), by = 7), function(i) sum(deaths1[i:(i + 6)]))

deaths2 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup %in% c( "2")) %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths2Week <- sapply(seq(1, length(deaths1), by = 7), function(i) sum(deaths2[i:(i + 6)]))

deaths3 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup %in% c("3")) %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths3Week <- sapply(seq(1, length(deaths3), by = 7), function(i) sum(deaths3[i:(i + 6)]))

deaths4 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup %in% c("4")) %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths4Week <- sapply(seq(1, length(deaths4), by = 7), function(i) sum(deaths4[i:(i + 6)]))

deaths5 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup == "5") %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths5Week <- sapply(seq(1, length(deaths5), by = 7), function(i) sum(deaths5[i:(i + 6)]))

deaths6 <- sol %>% filter(state %in% c("DRSV", "DSBI"), ageGroup == "6") %>%
  group_by(time) %>% summarise(n = sum(n)) %>%
  mutate(dailyDeaths = n - lag(n)) %>% select(dailyDeaths) %>% pull()
deaths6Week <- sapply(seq(1, length(deaths6), by = 7), function(i) sum(deaths6[i:(i + 6)]))


# Prepare the deathData and deathComb tables
deathData <- cases %>%
  select(startDate, ageGroup, deaths) %>%
  pivot_wider(id_cols = startDate, names_from = ageGroup, values_from = deaths) %>%
  select(!startDate) %>%
  mutate(from = "data", time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "7 days"))

deathData <- deathData %>%
  mutate(
    "1" = `1`,
    "2" = `2`,
    "3" = `3` ,
    "4" = `4`,
    "5" = `5`,
    "6" = `6`
  ) %>%
  # Keep only the necessary columns and add `from` and `time` columns
  select(time, "1", "2", "3", "4", "5", "6") %>%
  mutate(from = "data")

deathComb <- tibble(
  time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "7 days"),
  "1" = deaths1Week,
  "2" = deaths2Week,
  "3" = deaths3Week,
  "4" = deaths4Week,
  "5" = deaths5Week,
  "6" = deaths6Week,
  from = "est"
)

#Adding deaths data
deathComb <- deathComb %>% add_row(deathData)

deathComb <- deathComb %>% pivot_longer(cols = !c(time,from), names_to = "ageGroup", values_to = "n")

deathCombPlot <- deathComb %>% ggplot(aes(x = time, y = n, col = from, group = from))+
  geom_line()+
  geom_point(size = 0.5)+
  scale_color_manual(values = colors[c(3,5)])+
  facet_wrap(~ageGroup, labeller = as_labeller(c(
    "1" = "<1 years",
    "2" = "1-5 years", 
    "3" = "6-14 years",
    "4" = "15-44 years",
    "5" = "45-64 years",
    "6" = "65+ years")))+
  theme_bw(base_size = 12)+
  scale_x_date(date_labels = "%d %b")+
  labs(title = "B. Weekly deaths", x = "", y = "", col = "")+
  theme(legend.position = "none")
print(deathCombPlot)
ggsave(deathCombPlot,filename = "Plots/deathComp.png", height = 3.16, width = 7)

#Total number of hospitalization and deaths from data and from model
result <- sol %>% filter(state %in% c("HRSVCumu","HSpCumu","HSaCumu","HMpCumu", "DRSV","DSBI"), time == as.Date("2024-05-19")) %>% 
  #mutate(ageGroup = case_when(ageGroup %in% c("2","3","4") ~ "2,3,4",
                              #TRUE ~ ageGroup)) %>% 
  group_by(time,ageGroup, state) %>% summarise(n = sum(n)) %>% arrange(state)

SSI <- cases %>% filter(startDate == as.Date("2024-05-13")) %>% select(startDate, ageGroup, cumHosp, cumDeath)

library(writexl)
write_xlsx(result, "Plots/SBIResults.xlsx")
write_xlsx(SSI, "Plots/SSIData.xlsx")
