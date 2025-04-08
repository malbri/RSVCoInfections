rm(list = ls())

#Libraries
library(deSolve)
library(tidyverse)
library(lubridate)
library(patchwork)

#Load SEIR solution
sol <- readRDS(file = "Output/rsvModelSol2.rds")
perc <- readRDS(file = "Output/rsvModelPerc2.rds")
optim <- readRDS(file = "Output/rsvModelOptim2.rds")

#Colors
colors <- c("#D95F02", "#7570B3", "#E7298A", "#A76854", "#66A61E", "#E6AB02")

#Parameters
#xi - marternal immunity
round(optim$par[1],3)

#Inital infected
round(optim$par[2], 1)
round(optim$par[3], 1)
round(optim$par[4], 1)
round(optim$par[5], 1)
#relative suceptability - beta
round(optim$par[6], 3)
round(optim$par[7], 3)
round(optim$par[8], 3)
round(optim$par[9], 3)
round(optim$par[10], 3)
round(optim$par[11], 3)
#percent doctor
round(optim$par[12], 3)
round(optim$par[13], 3)
round(optim$par[14], 3)
round(optim$par[15], 3)
#round(optim$par[16], 3)*100

#extract age group and state 
sol <- sol %>%
  separate(
    state,
    into = c("state", "ageGroup"),
    sep = "(?<=\\D\\d?)(?=\\d$)",  # Adjusted regex to handle numbers within compartment names
    remove = FALSE
  ) %>%
  mutate(
    state = str_replace(state, "E\\d+", "E"),  # Replace E1, E2, etc., with E
    state = str_replace(state, "I\\d+", "I"),  # Replace I1, I2, etc., with I
  ) %>%
  group_by(time, state, ageGroup) %>%
  summarise(n = sum(n), .groups = "drop")

#library(writexl)
#write_xlsx(sol, "sol.xlsx")

#Load augmented file of influenza cases
cases <- readRDS(file = "Data/casesValidated.rds")
#write_xlsx(cases, "cases.xlsx")

#Calculate cumulative confirmed cases, to compare to R state
cases <- cases %>% filter(startDate >= as.Date("2023-09-18"), startDate <= as.Date("2024-05-13"))%>% 
  group_by(ageGroup) %>% mutate(cumConfirmed = cumsum(confirmed)) %>% ungroup() %>% 
  arrange(ageGroup)

#Validation: Peak time of the estimated cases
peakTimeData <- cases %>% group_by(ageGroup) %>% summarise(maxConfir = max(confirmed), maxConfiDate = startDate[which.max(confirmed)]) %>% 
  mutate(daysAfterStart = maxConfiDate - as.Date("2023-09-18")) %>% mutate(daysAfterStart = as.double(daysAfterStart)) %>% 
  select(daysAfterStart) %>% pull()/7

I1 <- sol %>% filter(ageGroup == "1", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()
I2 <- sol %>% filter(ageGroup == "2", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()
I3 <- sol %>% filter(ageGroup == "3", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()
I4 <- sol %>% filter(ageGroup == "4", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()
I5 <- sol %>% filter(ageGroup == "5", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()
I6 <- sol %>% filter(ageGroup == "6", state == "NewC") %>% group_by(time) %>% summarise(n = sum(n)) %>% select(n) %>% pull()

I1Week <- sapply(seq(1, length(I1), by = 7), function(i) sum(I1[i:(i+6)]))
I2Week <- sapply(seq(1, length(I2), by = 7), function(i) sum(I2[i:(i+6)]))
I3Week <- sapply(seq(1, length(I3), by = 7), function(i) sum(I3[i:(i+6)]))
I4Week <- sapply(seq(1, length(I4), by = 7), function(i) sum(I4[i:(i+6)]))
I5Week <- sapply(seq(1, length(I5), by = 7), function(i) sum(I5[i:(i+6)]))
I6Week <- sapply(seq(1, length(I6), by = 7), function(i) sum(I6[i:(i+6)]))

#Visualization of peak time
peakData <- tibble(weekStartDate = cases$startDate,
                   weeklyCasesSim = c(I1Week,I2Week,I3Week,I4Week,I5Week,I6Week),
                   weeklyCasesData = cases$confirmed,
                   ageGroup = rep(c("1","2","3","4","5","6"), each = 35)) 

peakSimPlot <- peakData %>% ggplot(aes(x = weekStartDate, y  = weeklyCasesSim))+
  facet_wrap(~ageGroup, labeller = as_labeller(c(
    "1" = "<1 year",
    "2" = "1-5 year",
    "3" = "6-14 year",
    "4" = "15-44 years",
    "5" = "45-64 years",
    "6" = "65+ years"
  ))) +
  geom_line(aes(col = ageGroup)) +
  scale_color_manual(values = colors[1:6])+
  
  # Add vertical lines for each age group peak time
  geom_vline(data = filter(peakData, ageGroup == "1"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[1] * 7)), col = colors[1], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "2"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[2] * 7)), col = colors[2], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "3"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[3] * 7)), col = colors[3], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "4"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[4] * 7)), col = colors[4], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "5"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[5] * 7)), col = colors[5], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "6"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[6] * 7)), col = colors[6], linetype = "dashed") +
  
  
  theme_bw(base_size = 12)+
  theme(legend.position="none",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,0,0))+
  labs(title = "A. Weekly cases estimated by the RSV model", x = "", y = "")+
  scale_x_date(date_labels = "%d %b")+
  scale_y_continuous(labels = scales::comma)

peakDataPlot <- peakData %>% ggplot(aes(x = weekStartDate, y  = weeklyCasesData))+
  facet_wrap(~ageGroup, labeller = as_labeller(c(
    "1" = "<1 year",
    "2" = "1-5 year",
    "3" = "6-14 year",
    "4" = "15-44 years",
    "5" = "45-64 years",
    "6" = "65+ years"
  ))) +
  geom_line(aes(col = ageGroup)) +
  scale_color_manual(values = colors[1:6])+
  # Add vertical lines for each age group peak time
  geom_vline(data = filter(peakData, ageGroup == "1"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[1] * 7)), col = colors[1], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "2"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[2] * 7)), col = colors[2], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "3"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[3] * 7)), col = colors[3], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "4"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[4] * 7)), col = colors[4], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "5"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[5] * 7)), col = colors[5], linetype = "dashed") +
  geom_vline(data = filter(peakData, ageGroup == "6"), aes(xintercept = as.Date(peakData$weekStartDate[1] + peakTimeData[6] * 7)), col = colors[6], linetype = "dashed") +
  
  theme_bw(base_size = 12)+
  theme(legend.position="none")+
  scale_x_date(date_labels = "%d %b")+
  labs(title = "B. Weekly confirmed cases", x = "", y = "")+
  scale_y_continuous(labels = scales::comma)

peakPlot <- peakSimPlot/peakDataPlot
print(peakPlot)
ggsave(peakPlot,filename = "Plots/peakTime.png")

#Confirmed cases vs ascertainment probabilities * estimated cases
cases %>% filter(startDate == as.Date("2024-05-13")) %>% select(ageGroup,cumConfirmed)

#Estimated cases by the model and Ascertainment prob * estimated cases
sol %>% filter(state == "C", time == as.Date("2024-05-13")) %>% 
  mutate(ascertN = case_when(ageGroup == "1" ~ n*perc[1],
                             ageGroup == "2" ~ n*perc[2],
                             ageGroup == "3" ~ n*perc[3],
                             ageGroup == "4" ~ n*perc[3],
                             ageGroup == "5" ~ n*perc[3],
                             ageGroup == "6" ~ n*perc[4],
                             TRUE ~ NA)) 

