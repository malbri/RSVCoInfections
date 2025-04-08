rm(list = ls())
setwd("C:/Users/malbri/OneDrive - Danmarks Tekniske Universitet/Dokumenter/4_RSV/RSV_project_2")

library(deSolve)
library(tidyverse)
library(readxl)
library(writexl)

#load Parameter spans
paramSpan <- readRDS(file = "Output//Scenarios.rds")

#Model period
timePeriod <- 245
tVal <- seq(1,timePeriod)

#Load eta values (percent of hospitalizations due to RSV, RSV + S. p and RSV + S. a)
etaRSV <- readRDS(file = "Data/04EtaRSV.rds")
etaMp <- readRDS(file = "Data/04EtaMp.rds")
etaSa <- readRDS(file = "Data/04EtaSa.rds")
etaSp <- readRDS(file = "Data/04EtaSp.rds")

#Dataframe for outcomes
RSVresult <- list()
SBIresult <- list()
outcomes <- tibble(nRSV = numeric(),
                   nHospRSV = numeric(),
                   nDeathsRSV = numeric(),
                   nHospSBI = numeric(),
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
  RSVresult[[runN]] <- sol[245,44:49]
  
  solution <- tibble(time = tVal)
  
  for (state in 2:ncol(sol)){
    colName <- colnames(sol)[state]
    solution <- solution %>% mutate(!!colName := sol[,state])
  }
  
  nRSV <- solution %>% filter(time == 245) %>% 
    select(matches("C")) %>% sum() %>% ceiling()
  
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
  
  SBIresult[[runN]] <- out[245,32:67]

  #Extract solution
  solutionCO <- tibble(time = tVal)
  
  for (state in 2:ncol(out)){
    colName <- colnames(out)[state]
    solutionCO <- solutionCO %>% mutate(!!colName := out[,state])
  }
  
  #calculate number of SBI hospitalizations and SBI deaths
  nHospSBI <- solutionCO %>% filter(time == 245) %>% 
    select(matches("^HSpCumu|^HSaCumu|^HMpCumu")) %>% sum() %>% ceiling()
  
  nDeathsSBI <- solutionCO %>% filter(time == 245) %>% 
    select(starts_with("DS")) %>% sum() %>% ceiling()
  
  nHospRSV <- solutionCO %>% filter(time == 245) %>% 
    select(matches("^HRSVCumu")) %>% sum() %>% ceiling()
  
  nDeathsRSV <- solutionCO %>% filter(time == 245) %>% 
    select(starts_with("DR")) %>% sum() %>% ceiling()
 
  #Add to outcome data frame
  outcomes <- outcomes %>% add_row(nRSV, nHospRSV, nHospSBI, nDeathsRSV, nDeathsSBI)
  
  print(paste("SBI model number",runN,"done"))
}

# Convert list to a data frame
result_RSV <- do.call(rbind, RSVresult)
result_HOS <- do.call(rbind, SBIresult)
result_RSV <- as.data.frame(result_RSV)
result_HOS <- as.data.frame(result_HOS)

saveRDS(result_RSV, file = "Output/result_RSV.rds")
saveRDS(result_HOS, file = "Output/result_HOS.rds")

# Loop through the age groups and create the new variables
for (i in 1:6) {
  result_HOS[[paste0("nHospSBI", i)]] <- 
    result_HOS[[paste0("HSpCumu", i)]] +
    result_HOS[[paste0("HSaCumu", i)]] +
    result_HOS[[paste0("HMpCumu", i)]]
}

# Loop through the age groups and calculate the total antibiotic usage
for (i in 1:6) {
  # Define the percentages for antibiotic usage
  if (i <= 3) {
    RSV_percent <- 0.718  # 71.8% for age groups 1-3
    SBI_percent <- 0.856  # 85.6% for age groups 1-3
  } else {
    RSV_percent <- 0.88   # 88% for age groups 4-6
    SBI_percent <- 0.88   # 88% for age groups 4-6
  }
  
  # Calculate the total antibiotic usage for each group
  result_HOS[[paste0("RSV_antibiotics_", i)]] <- result_HOS[[paste0("HRSVCumu", i)]] * RSV_percent
  result_HOS[[paste0("SBI_antibiotics_", i)]] <- result_HOS[[paste0("nHospSBI", i)]] * SBI_percent
}


# Create a vector of column names that contain the relevant antibiotic data
columns_rsv <- paste0("RSV_antibiotics_", 1:6)
columns_sbi <- paste0("SBI_antibiotics_", 1:6)
# Combine the column names
all_columns <- c(columns_rsv, columns_sbi)

# Calculate the sum of antibiotics for each strategy (row)
result_HOS$sum_antibiotics <- rowSums(result_HOS[all_columns], na.rm = TRUE)

antibiotics <- result_HOS$sum_antibiotics

#write_xlsx(result_HOS,"Output/agestratified_hos.xlsx")

strategies <- c("baseline","1","2","3","4","5","6","7","8","9","10","11","12","13","14")
strategies <- data.frame(strategy = strategies)
AddColoumn <- cbind(strategies, antibiotics)
paramOutcomes <- AddColoumn %>% add_column(outcomes)


saveRDS(paramOutcomes, file = "Output/OutcomesStrategies.rds")

