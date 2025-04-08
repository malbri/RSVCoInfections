rm(list = ls())

library(SimInf)
library(deSolve)

#load Parameter spans
paramSpan <- readRDS(file = "data/solutionData/07paramSpan.rds")

#Number of simulations of the influenza model to run for each set of parameters
n <- 50

#Model period
timePeriod <- 259

#Load eta values (percent of hospitalizations due to Influenza, Influenza + S. p and Influenza + S. a)
etaInfl <- readRDS(file = "data/inputDataSimulated/04aEtaInfl.rds")
etaSa <- readRDS(file = "data/inputDataSimulated/04aEtaSa.rds")
etaSp <- readRDS(file = "data/inputDataSimulated/04aEtaSp.rds")

#Dataframe for outcomes
outcomes <- tibble(nHospSBI = numeric(),
                   nDeathsSBI = numeric())

for (runN in 1:nrow(paramSpan)){
  #Load scripts
  #Parameters
  source("scripts/07_sensitivityAnalysis/InfluenzaModelParameters.R")
  
  #Initial conditions
  source("scripts/07_sensitivityAnalysis/InfluenzaModelInitial.R")
  
  #Transitions (equations) and compartments
  source("scripts/03b_InfluenzaModel/InfluenzaModelTransitions.R")
  
  #Vaccine data
  source("scripts/03b_InfluenzaModel/InfluenzaModelVaccines.R")
  
  #Define model
  model <- mparse(transitions = transitions, compartments = compartments,
                  gdata = c(sigma1 = paramSpan$sigma1[runN], sigma2 = paramSpan$sigma2[runN], 
                            sigma3 = paramSpan$sigma3[runN], sigma4 = paramSpan$sigma4[runN], 
                            sigma5 = paramSpan$sigma5[runN],
                            c11 = c[1,1], c12 = c[1,2], c13 = c[1,3], c14 = c[1,4], c15 = c[1,5],
                            c21 = c[2,1], c22 = c[2,2], c23 = c[2,3], c24 = c[2,4], c25 = c[2,5],
                            c31 = c[3,1], c32 = c[3,2], c33 = c[3,3], c34 = c[3,4], c35 = c[3,5],
                            c41 = c[4,1], c42 = c[4,2], c43 = c[4,3], c44 = c[4,4], c45 = c[4,5],
                            c51 = c[5,1], c52 = c[5,2], c53 = c[5,3], c54 = c[5,4], c55 = c[5,5],
                            totPop = totPop, nu = paramSpan$nu[runN],
                            lambda1 = paramSpan$lambda1[runN], lambda2 = paramSpan$lambda2[runN], 
                            lambda3 = paramSpan$lambda3[runN], lambda4 = paramSpan$lambda4[runN], 
                            lambda5 = paramSpan$lambda5[runN],
                            delta1 = paramSpan$delta1[runN], delta2 = paramSpan$delta2[runN],
                            delta3 = paramSpan$delta3[runN], delta4 = paramSpan$delta4[runN],
                            delta5 = paramSpan$delta5[runN]), 
                  u0 = u0, tspan = 1:259, events = vacc, E = E, N = N)
  
  #Run SEIR model
  result <- run(model = model)
  
  #Save the data from the model
  sol <- trajectory(model = result)
  
  #Create longer dataframe for results
  sol <- sol %>% pivot_longer(!c(time,node), names_to = "state", values_to = "n")
  
  #Calculate mean of the 50 runs
  sol <- sol %>% group_by(time,state) %>% summarise(meanVal = mean(n),
                                                    minVal = min(n),
                                                    maxVal = max(n))
  
  print(paste("Influenza model number",runN,"done"))
  
  #Extract I2V and I2N
  IresultNmean <- sol %>% 
    filter(str_detect(state,"I2_N")) %>% ungroup() %>% select(time,state,meanVal) %>% 
    pivot_wider(names_from = state, values_from = meanVal) %>% select(-time) %>% as.matrix()
  
  IresultVmean <- sol %>% 
    filter(str_detect(state,"I2_V")) %>% ungroup() %>% select(time,state,meanVal) %>% 
    pivot_wider(names_from = state, values_from = meanVal) %>% select(-time) %>% as.matrix()
  
  #Start SBI model
  #Equations
  source("scripts/04b_SBImodel/SBIequations.R")
  
  #Parameters
  source("scripts/07_sensitivityAnalysis/SBIparametersAndInitial.R")
  
  phi = c(paramSpan$phi1[runN],paramSpan$phi2[runN],paramSpan$phi3[runN],
          paramSpan$phi4[runN],paramSpan$phi5[runN])
  
  alpha = c(paramSpan$alpha1[runN],paramSpan$alpha2[runN],paramSpan$alpha3[runN],
            paramSpan$alpha4[runN],paramSpan$alpha5[runN])
  
  #Calculate infected when subtracting hospitalized individuals
  tempN <- matrix(data = 0, nrow = timePeriod, ncol = 5)
  tempV <- matrix(data = 0, nrow = timePeriod, ncol = 5)
  
  tempN[1,] <- IresultNmean[1,]
  tempV[1,] <- IresultVmean[1,]
  
  for (i in 2:timePeriod){
    tempN[i,] <- IresultNmean[i,] - phi*IresultNmean[i-1,]
    tempV[i,] <- IresultVmean[i,] - alpha*phi*IresultVmean[i-1,]
    
    for (j in 1:5){
      if(tempN[i,j] < 0){
        tempN[i,j] = 0
      }
      if(tempV[i,j] < 0){
        tempV[i,j] = 0
      }
    }
  }
  
  IresultN <- tempN
  IresultV <- tempV
  
  tVal <- seq(1,timePeriod) # days. 
  
  #Solving ODEs
  out <- ode(
    y = init,
    times = tVal,
    func = SBIequ,
    parms = paramVal
  )
  
  #Extract solution
  solution <- tibble(time = tVal)
  
  for (state in 2:ncol(out)){
    colName <- colnames(out)[state]
    solution <- solution %>% mutate(!!colName := out[,state])
  }
  
  #calculate number of SBI hospitalizations and SBI deaths
  nHospSBI <- solution %>% filter(time == 259) %>% 
    select(contains("Cumu")) %>% select(starts_with("HS")) %>% sum() %>% ceiling()
  
  nDeathsSBI <- solution %>% filter(time == 259) %>% 
    select(starts_with("DS")) %>% sum() %>% ceiling()
    
  #Add to outcome data frame
  outcomes <- outcomes %>% add_row(nHospSBI, nDeathsSBI)
  
  print(paste("SBI model number",runN,"done"))
}

paramOutcomes <- paramSpan %>% add_column(outcomes)

saveRDS(paramOutcomes, file = "data/solutionData/07paramOutcomes.rds")
