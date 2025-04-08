setwd("C:/Users/malbri/OneDrive - Danmarks Tekniske Universitet/Dokumenter/4_RSV/RSV_project_2")
rm(list = ls())
set.seed(123)

#Libraries
library(deSolve)
library(tidyverse)
library(hhh4contacts)
library(dfoptim)

####PARAMETERS####
#Load infectious individuals from the RSV model
Iresult <- readRDS(file = "Output/Iresultmean.rds")
#Iresult <- Iresult[-1,]

#Load eta values
etaRSV <- readRDS(file = "Data/04EtaRSV.rds")
etaMp <- readRDS(file = "Data/04EtaMp.rds")
etaSa <- readRDS(file = "Data/04EtaSa.rds")
etaSp <- readRDS(file = "Data/04EtaSp.rds")

#Model period
timePeriod <- 245

####Write simulator####
SBImodelSim <- function(phi1, phi2, phi34, phi5, phi6, 
                        mu12, mu3, mu4, mu5, mu6) {
  
  #Put parameters into vectors
  #hospitalization rate
  phi  = c(phi1,phi2,phi34,phi34,phi5,phi6)
  
  #Calculate infected when subtracting hospitalized individuals
  tempN <- matrix(data = 0, nrow = timePeriod, ncol = 6)
  
  tempN[1,] <- Iresult[1,]
  
  for (i in 2:timePeriod) {
    tempN[i,] <- Iresult[i,] - phi * Iresult[i-1,]
    
    for (j in 1:6) {
      if (tempN[i,j] < 0) {
        tempN[i,j] = 0
      }
    }
  }
  Iresult <- tempN
  
  # the differential equations:
  SBIequ <- function(t, state, param) {
    #print(list(t = t, state = state, param = param))
    #print(list(t = t, Iresult = Iresult[t,], phi = phi, etaRSV = etaRSV))
  
    #Put parameters into vectors
    #hospitalization rate
    phi  = c(phi1,phi2,phi34,phi34,phi5,phi6)
    
    #Death rate
    mu  = c(mu12,mu12,mu3,mu4,mu5,mu6)
    
    #n age groups
    n <- 6
    
    #States
    HRSV <- state[1:n]
    HSp <- state[n + (1:n)]
    HSa <- state[2*n + (1:n)]
    HMp <- state[3*n + (1:n)]
    R <- state[4*n + (1:n)]
    DRSV <- state[5*n + (1:n)]
    DSBI <- state[6*n + (1:n)]
    
    #States containing cumulative hospitalization
    HRSVCumu <- state[7*n + (1:n)]
    HSpCumu <- state[8*n + (1:n)]
    HSaCumu <- state[9*n + (1:n)]
    HMpCumu <- state[10*n + (1:n)]
    
    
    dHRSV <- numeric(n)
    dHSp <- numeric(n)
    dHSa <- numeric(n)
    dHMp <- numeric(n)
    dR <- numeric(n)
    dDRSV <- numeric(n)
    dDSBI <- numeric(n)
    dHRSVCumu <- numeric(n)
    dHSpCumu <- numeric(n)
    dHSaCumu <- numeric(n)
    dHMpCumu <- numeric(n)
    
    #Update I subtracting the individuals that have been hospitalized
    with(as.list(c(param)), {
      for (i in 1:n){
        
        #Hospitalized with only RSV
        dHRSV[i] <- phi[i]*Iresult[t,i]*etaRSV[i] - gammaRSV[i]*HRSV[i] - mu[i]*HRSV[i]
        
        #Hospitalized with RSV and S. pneumonia 
        dHSp[i] <- phi[i]*Iresult[t,i]*etaSp[i] - gammaSBI[i]*HSp[i] - mu[i]*epsilonSBI[i]*HSp[i]
        
        #Hospitalized with RSV and S. aureus
        dHSa[i] <- phi[i]*Iresult[t,i]*etaSa[i] - gammaSBI[i]*HSa[i] - mu[i]*epsilonSBI[i]*HSa[i]
        
        #Hospitalized with RSV and M. pneumonia 
        dHMp[i] <- phi[i]*Iresult[t,i]*etaMp[i] - gammaSBI[i]*HMp[i] - mu[i]*epsilonSBI[i]*HMp[i]
        
        #Recovery
        dR[i] <- gammaRSV[i]*HRSV[i] + gammaSBI[i]*(HSp[i] + HSa[i] + HMp[i])
        
        #Death
        dDRSV[i] <- mu[i]*HRSV[i]
        dDSBI[i] <- mu[i]*(epsilonSBI[i]*(HSp[i] + HSa[i] + HMp[i]))
        
        #Extra states to capture cumulative hospitalization
        dHRSVCumu[i] <- phi[i]*Iresult[t,i]*etaRSV[i]
        dHSpCumu[i] <- phi[i]*Iresult[t,i]*etaSp[i]
        dHSaCumu[i] <- phi[i]*Iresult[t,i]*etaSa[i]
        dHMpCumu[i] <- phi[i]*Iresult[t,i]*etaMp[i]
      }
      #print(paste("Cumu:", phi[i], Iresult[t,i], etaRSV[i]))
      
      return(list(c(dHRSV, dHSp, dHSa, dHMp, dR, dDRSV, dDSBI, dHRSVCumu, dHSpCumu, dHSaCumu, dHMpCumu)))
     
    })
  }
  
  # the parameters values:
  paramVal <- list(
    #Hospitalization rate
    phi1 = phi1,
    phi2 = phi2,
    phi34 = phi34,
    phi5 = phi5,
    phi6 = phi6,
    
    #Recovery rate from RSV hospitalization
    gammaRSV = c(1/5, 1/5, 1/5, 1/5, 1/5, 1/5), 
    gammaSBI = c(1/5, 1/5, 1/5, 1/5, 1/5, 1/5)*c(1/1.16,1/1.16,1/1.16,1/1.16,1/1.16,1/1.16), 
    
    #Death rate
    mu12 = mu12,
    mu3 = mu3,
    mu4 = mu4,
    mu5 = mu5,
    mu6 = mu6,
    
    
    #factor up scaling deaths are due to RSV + SBI hospitalizations
    epsilonSBI = c(1,1,1,1,2,2) 
  )
  
  # the initial values of variables:
  init <- c(
    HRSV = rep(1e-6, 6),  # Avoid exact zeros
    HSp = rep(1e-6, 6),
    HSa = rep(1e-6, 6),
    HMp = rep(1e-6, 6),
    R = rep(1e-6, 6),
    DRSV = rep(0, 6),
    DSBI = rep(0, 6),
    HRSVCumu = rep(0, 6),
    HSpCumu = rep(0, 6),
    HSaCumu = rep(0, 6),
    HMpCumu = rep(0, 6)
  )
  
  #t
  tVal <- seq(1,timePeriod) # days. 
  
  #Solving ODEs
  out <- ode(
    y = init,
    times = tVal,
    func = SBIequ,
    parms = paramVal,
    rtol = 1e-3,
    atol = 1e-6
  )
  
  # returning the output:
  as.data.frame(out)
}

#Test it
predictions <- SBImodelSim(0.01, 0.0009, 0.00003, 0.0002, 0.0006, 
                           0.0099, 0.001, 0.008, 0.01, 0.074)

#Load RSV cases data
data <- readRDS(file = "Data/casesValidated.rds")

#Only extract the 39 weeks from week 33 2023 till week 20 2024
hospitalData <- data %>% filter(startDate >= as.Date("2023-09-18"), startDate <= as.Date("2024-05-13")) %>% 
  group_by(ageGroup) %>% mutate(cumHosp = cumsum(hospitalizations)) %>% ungroup() %>% 
  select(startDate,ageGroup,cumHosp)


deathData <- data %>% filter(startDate >= as.Date("2023-09-18"), startDate <= as.Date("2024-05-13")) %>% 
  group_by(ageGroup) %>% mutate(cumDeath = cumsum(deaths)) %>% ungroup() %>% 
  select(startDate,ageGroup,cumDeath)

#Objective function to be optimized
objFunc <- function(phi1, phi2, phi34, phi5, phi6,
                    mu12, mu3, mu4, mu5, mu6) {
  

  #Run SEIR model
  predictions <- SBImodelSim(phi1, phi2, phi34, phi5, phi6,
                             mu12, mu3, mu4, mu5, mu6)
  
  ## Optimization of hospitalization rates
  
  # Predicted hospitalizations for the new age groups
  hosp1 <- last(predictions$HRSVCumu1 + predictions$HMpCumu1 + predictions$HSpCumu1 + predictions$HSaCumu1)
  hosp2 <- last(predictions$HRSVCumu2 + predictions$HMpCumu2 + predictions$HSpCumu2 + predictions$HSaCumu2)
  hosp3 <- last(predictions$HRSVCumu3 + predictions$HMpCumu3 + predictions$HSpCumu3 + predictions$HSaCumu3)
  hosp4 <- last(predictions$HRSVCumu4 + predictions$HMpCumu4 + predictions$HSpCumu4 + predictions$HSaCumu4)
  hosp5 <- last(predictions$HRSVCumu5 + predictions$HMpCumu5 + predictions$HSpCumu5 + predictions$HSaCumu5)
  hosp6 <- last(predictions$HRSVCumu6 + predictions$HMpCumu6 + predictions$HSpCumu6 + predictions$HSaCumu6)

  
  # Hospitalizations according to data for the new age groups
  hosp1data <- hospitalData %>% filter(ageGroup == "1", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  hosp2data <- hospitalData %>% filter(ageGroup == "2", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  hosp3data <- hospitalData %>% filter(ageGroup == "3", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  hosp4data <- hospitalData %>% filter(ageGroup == "4", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  hosp5data <- hospitalData %>% filter(ageGroup == "5", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  hosp6data <- hospitalData %>% filter(ageGroup == "6", startDate == as.Date("2024-05-13")) %>% select(cumHosp) %>% pull()
  
  # SSE between the predicted hospitalizations and the true hospitalizations
  SSEhosp1 <- (hosp1data - hosp1)^2
  SSEhosp2 <- (hosp2data - hosp2)^2
  SSEhosp3 <- (hosp3data - hosp3)^2
  SSEhosp4 <- (hosp4data - hosp4)^2
  SSEhosp5 <- (hosp5data - hosp5)^2
  SSEhosp6 <- (hosp6data - hosp6)^2
  
  ## Optimization of number of deaths
  
  # Predicted deaths for the new age groups
  deaths1 <- last(predictions$DRSV1 + predictions$DSBI1)
  deaths2 <- last(predictions$DRSV2 + predictions$DSBI2)
  deaths3 <- last(predictions$DRSV3 + predictions$DSBI3)
  deaths4 <- last(predictions$DRSV4 + predictions$DSBI4)
  deaths5 <- last(predictions$DRSV5 + predictions$DSBI5)
  deaths6 <- last(predictions$DRSV6 + predictions$DSBI6)

  
  # Deaths according to data for the new age groups
  deaths1data <- deathData %>% filter(ageGroup == "1", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  deaths2data <- deathData %>% filter(ageGroup == "2", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  deaths3data <- deathData %>% filter(ageGroup == "3", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  deaths4data <- deathData %>% filter(ageGroup == "4", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  deaths5data <- deathData %>% filter(ageGroup == "5", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  deaths6data <- deathData %>% filter(ageGroup == "6", startDate == as.Date("2024-05-13")) %>% select(cumDeath) %>% pull()
  
  # SSE between the predicted deaths and the true deaths
  SSEdeaths1 <- (deaths1data - deaths1)^2
  SSEdeaths2 <- (deaths2data - deaths2)^2
  SSEdeaths3 <- (deaths3data - deaths3)^2
  SSEdeaths4 <- (deaths4data - deaths4)^2
  SSEdeaths5 <- (deaths5data - deaths5)^2
  SSEdeaths6 <- (deaths6data - deaths6)^2
  
  # Sum of all SSEs for hospitalizations and deaths
  total_SSE <- sum(SSEhosp1, SSEhosp2, SSEhosp3, SSEhosp4, SSEhosp5, SSEhosp6,
                   SSEdeaths1, SSEdeaths2, SSEdeaths3, SSEdeaths4, SSEdeaths5, SSEdeaths6)
  
  print(total_SSE)
}

#test it
objFunc(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2, 0.2)

#Optimization Function
objFuncOptim <- function(parameters) {
  objFunc(phi1 = parameters[1], phi2 = parameters[2], phi34 = parameters[3], phi5 = parameters[4], phi6 = parameters[5],
          mu12 = parameters[6], mu3 =parameters[7] , mu4 = parameters[8], mu5 = parameters[9], mu6 = parameters[10])
  
}


# Starting values, lower and upper limits
startingVal <- c(5e-4, 5e-4, 5e-5, 1e-4, 1e-3,  # hospitalization rates
                 5e-5, 5e-5, 5e-5, 5e-3, 5e-3) # death rates

lower <- c(1e-4, 1e-5, 1e-6, 1e-5, 1e-5, # hospitalization rates 
           1e-6, 1e-6, 1e-6, 1e-6, 1e-6) # death rates

upper <- c(1e-1, 1e-3, 1e-3, 1e-3, 1e-2,  # hospitalization rates
           1e-4, 1e-4, 1e-4, 1e-2, 2e-1) # death rates


#Optimization procedure
startTime <- Sys.time()
control_list <- list(maxfeval = 10000, tol = 1e-6, trace = 1)
optimSBI <- nmkb(startingVal, objFuncOptim,
              lower = lower,
              upper = upper,
              control = control_list)

endTime <- Sys.time()
endTime - startTime
print(optimSBI)

#hospital rate
round(optimSBI$par[1],5)
round(optimSBI$par[2],5)
round(optimSBI$par[3],5)
round(optimSBI$par[4],5)
round(optimSBI$par[5],5)
#death rate
round(optimSBI$par[6],6)
round(optimSBI$par[7],6)
round(optimSBI$par[8],6)
round(optimSBI$par[9],6)
round(optimSBI$par[10],6)

#Save optimized parameters
saveRDS(optimSBI, file = "Output/SBIoptim.rds")

#Result using optimized parameter values
sol <- SBImodelSim(phi1 = optimSBI$par[1], phi2 = optimSBI$par[2], phi34 = optimSBI$par[3], phi5 = optimSBI$par[4], phi6 = optimSBI$par[5],
                   mu12 = optimSBI$par[6], mu3 =optimSBI$par[7] , mu4 = optimSBI$par[8], mu5 = optimSBI$par[9], mu6 = optimSBI$par[10])


#Put into tibble
solution <- tibble(time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "1 day"))

for (i in 2:ncol(sol)){
  colName <- colnames(sol)[i]
  solution <- solution %>% mutate(!!colName := sol[,i])
}

#Create long data frame
solSBI <- solution %>% pivot_longer(!time, names_to = "state", values_to = "n") 

#Save solutions
saveRDS(solSBI, file = "Output/SBIsol.rds")

