rm(list = ls())
setwd("C:/Users/malbri/OneDrive - Danmarks Tekniske Universitet/Dokumenter/4_RSV/RSV_project_2")

library(deSolve)
library(tidyverse)
library(readxl)

#load Parameter spans
paramSpan <- readRDS(file = "Output//ParamSpan.rds")

#Model period
timePeriod <- 245
tVal <- seq(1,timePeriod)

#Load eta values (percent of hospitalizations due to RSV, RSV + S. p and RSV + S. a)
etaRSV <- readRDS(file = "Data/04EtaRSV.rds")
etaMp <- readRDS(file = "Data/04EtaMp.rds")
etaSa <- readRDS(file = "Data/04EtaSa.rds")
etaSp <- readRDS(file = "Data/04EtaSp.rds")

#Dataframe for outcomes
outcomes <- tibble(nHospSBI = numeric(),
                   nDeathsSBI = numeric())

for (runN in 1:nrow(paramSpan)){
  #Load scripts
  #Parameters and Initial conditions
  source("Scripts/Sensitivity/RSVModelInitial.R")
  
  #EQ
  source("Scripts/Sensitivity/EQs.R")
  
  #Run SEIR model
  sol <- ode(
    y = RSVinit,
    times = tVal,
    func = RSVequ,
    parms = RSVparamVal
  )
  
  solution <- tibble(time = tVal)
  
  for (state in 2:ncol(sol)){
    colName <- colnames(sol)[state]
    solution <- solution %>% mutate(!!colName := sol[,state])
  }
  
  #Create longer dataframe for results
  sol <- solution %>% pivot_longer(!time, names_to = "state", values_to = "n") 
  
  #Calculate mean of the 50 runs
  sol <- sol %>% group_by(time,state) %>% summarise(meanVal = mean(n),
                                                    minVal = min(n),
                                                    maxVal = max(n))
  
  print(paste("RSV model number",runN,"done"))

  #Extract I2
    IresultNmean <- sol %>% 
    filter(str_detect(state,"I2")) %>% ungroup() %>% select(time,state,meanVal) %>% 
    pivot_wider(names_from = state, values_from = meanVal) %>% select(-time) %>% as.matrix()
  

  #Start SBI model
  #Parameters
  source("Scripts/Sensitivity/SBIparametersAndInitial.R")
  
  phi = c(paramSpan$phi1[runN],paramSpan$phi2[runN],paramSpan$phi34[runN],paramSpan$phi34[runN],
            paramSpan$phi5[runN],paramSpan$phi6[runN])
  
  #Calculate infected when subtracting hospitalized individuals
  tempN <- matrix(data = 0, nrow = timePeriod, ncol = 6)

  tempN[1,] <- IresultNmean[1,]

  for (i in 2:timePeriod){
    tempN[i,] <- IresultNmean[i,] - phi*IresultNmean[i-1,]
    
    for (j in 1:6){
      if(tempN[i,j] < 0){
        tempN[i,j] = 0
      }
    }
  }
  
  Iresult <- tempN
 
  tVal <- seq(1,timePeriod) # days. 
  
  #Solving ODEs
  out <- ode(
    y = SBIinit,
    times = tVal,
    func = SBIequ,
    parms = SBIparamVal
  )

  #Extract solution
  solution <- tibble(time = tVal)
  
  for (state in 2:ncol(out)){
    colName <- colnames(out)[state]
    solution <- solution %>% mutate(!!colName := out[,state])
  }
  
  #calculate number of SBI hospitalizations and SBI deaths
  nHospSBI <- solution %>% filter(time == 245) %>% 
    select(contains("Cumu")) %>% select(starts_with("HS")) %>% sum() %>% ceiling()
  
  nDeathsSBI <- solution %>% filter(time == 245) %>% 
    select(starts_with("DS")) %>% sum() %>% ceiling()
    
  #Add to outcome data frame
  outcomes <- outcomes %>% add_row(nHospSBI, nDeathsSBI)
  
  print(paste("SBI model number",runN,"done"))
}

paramOutcomes <- paramSpan %>% add_column(outcomes)

saveRDS(paramOutcomes, file = "Output/paramOutcomes.rds")
