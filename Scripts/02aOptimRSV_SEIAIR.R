setwd("C:/Users/malbri/OneDrive - Danmarks Tekniske Universitet/Dokumenter/4_RSV/RSV_project_2")
rm(list = ls())

#Libraries
library(deSolve)
library(tidyverse)
library(dfoptim)
library(readxl)
set.seed(123)

###PARAMETERS###
#Load contact matrix
c <- readRDS(file = "Data/contactmatrix.rds")

###Population###
#load population
pop <- read_xlsx("Data/Population.xlsx")

#Total population size
N <- sum(pop$Number)


###Write simulator###
SEIRsim <- function(xi, In1, In2, In345, In6, beta1, beta2, beta3, beta4, beta5, beta6) {
  
  require(deSolve) # for the "ode" function
  
  # the differential equations:
  seirEqu <- function(t, state, param) {
    beta = c(beta1, beta2, beta3, beta4, beta5, beta6)
    
    #n age groups
    n <- 6
    
    #States
    S <- state[1:n]
    E1 <- state[n + (1:n)]
    E2 <- state[2*n + (1:n)]
    IA <- state[3*n + (1:n)]
    I1 <- state[4*n + (1:n)]
    I2 <- state[5*n + (1:n)]
    R <- state[6*n + (1:n)]
    C <- state[7*n + (1:n)]
    
    dS <- numeric(n)
    dE1 <- numeric(n)
    dE2 <- numeric(n)
    dIA <- numeric(n)
    dI1 <- numeric(n)
    dI2 <- numeric(n)
    dR <- numeric(n)
    dC <- numeric(n)
    
    # Initialize lambda as a numeric vector with length n
    lambda <- numeric(n)
    
    
    with(as.list(c(param)), {
      for (i in 1:n) {  
        
        if (i == 1){
          lambda[i] <- xi * beta[i] * ((
            c[i, 1] * (I1[1] + I2[1]) +
              c[i, 2] * (I1[2] + I2[2]) +
              c[i, 3] * (I1[3] + I2[3]) +
              c[i, 4] * (I1[4] + I2[4]) +
              c[i, 5] * (I1[5] + I2[5]) +
              c[i, 6] * (I1[6] + I2[6]) ) 
            + (alpha[i] * (
              c[i, 1] * IA[1] +
                c[i, 2] * IA[2] +
                c[i, 3] * IA[3] +
                c[i, 4] * IA[4] +
                c[i, 5] * IA[5] +
                c[i, 6] * IA[6]))
          )
        }
        
        else {
          lambda[i] <- beta[i] * ((
            c[i, 1] * (I1[1] + I2[1]) +
              c[i, 2] * (I1[2] + I2[2]) +
              c[i, 3] * (I1[3] + I2[3]) +
              c[i, 4] * (I1[4] + I2[4]) +
              c[i, 5] * (I1[5] + I2[5]) +
              c[i, 6] * (I1[6] + I2[6]) ) 
            + (alpha[i] * (
              c[i, 1] * IA[1] +
                c[i, 2] * IA[2] +
                c[i, 3] * IA[3] +
                c[i, 4] * IA[4] +
                c[i, 5] * IA[5] +
                c[i, 6] * IA[6] ))
          )
        }
        
        # Susceptible
        dS[i] <- -lambda[i] * S[i] / N
        
        # Exposed - 1
        dE1[i] <- lambda[i] * S[i] / N - sigma * E1[i]
        
        # Exposed - 2
        dE2[i] <- sigma * (E1[i]-E2[i])
        
        #Infected - asymptomatic
        dIA[i] <- rho[i] * sigma * E2[i] - gamma * IA[i]
        
        # Infected1 - symptomatic
        dI1[i] <- (1-rho[i]) * sigma * E2[i] - gamma * I1[i]
        
        # Infected2 - symptomatic
        dI2[i] <-  gamma * (I1[i]-I2[i])
        
        # Recovered 
        dR[i] <- gamma * I2[i] + gamma * IA[i]
        
        #Infected symptomatic cumulative
        dC[i] <- (1-rho[i]) * sigma * E2[i]
        
      }
      
      return(list(c(dS, dE1, dE2, dIA, dI1, dI2, dR, dC)))
    })
  }
  
  # the parameters values:
  paramVal <- list(
    #natural maternal immunity rate
    xi = xi,
    #relative susceptibility
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    beta4 = beta4,
    beta5 = beta5,
    beta6 = beta6,
    
    #Contracts
    c = c,
    #Latency rate 
    sigma = 0.5, 
    #Infectious period
    gamma = 0.2,
    
    #proportion asymptomatic
    rho = c(0.0916, 0.163, 0.516, 0.753, 0.753, 0.753),
    
    #smitte asymptomatic
    alpha = c(rep(0.634, 6))
  )
  
  # the initial values of variables:
  init <- c(
    S = pop$Number - c(In1, In2, In345, In345,In345, In6),
    E1 = rep(0,6),
    E2 = rep(0,6),
    IA = c(0.0916*In1, 0.163*In2, 0.516*In345, 0.753*In345, 0.753*In345, 0.753*In6),
    I1 = c((1-0.0916)*In1, (1-0.163)*In2, (1-0.516)*In345, (1-0.753)*In345, (1-0.753)*In345, (1-0.753)*In6),
    I2 = rep(0,6),
    R = rep(0,6),
    C= rep(0,6))
  
  
  #t
  tVal <- seq(1,245) # days
  
  #Solving ODEs
  out <- ode(
    y = init,
    times = tVal,
    func = seirEqu,
    parms = paramVal
  )
  
  # returning the output:
  as.data.frame(out)
}

#Test it - xi, In1, In2, In3, In4, In5, In6, beta1, beta2, beta3, beta4, beta5, beta6, rho1, rho2, rho3, rho456, alpha
predictions <- SEIRsim(0.45,
                       20, 180, 300, 100,
                       0.99, 0.2, 0.02, 0.55,
                       0.1,
                       0.2)

#Load RSV cases data
data <- readRDS(file = "Data/casesValidated_345.rds")

#Only extract the 37 weeks from week 38 2022 till week 22 2023
data <- data %>% filter(startDate >= as.Date("2023-09-18"), startDate <= as.Date("2024-05-13"))%>% 
  group_by(ageGroup) %>% mutate(cumConfirmed = cumsum(confirmed),
                                cumHosp = cumsum(hospitalizations),
                                cumDeaths = cumsum(deaths)) %>% ungroup()

#Objective function to be optimized
objFunc <- function(xi, 
                    In1, In2, In345, In6, 
                    beta1, beta2, beta3, beta4, beta5, beta6,
                    perc1, perc2, perc345, perc6) {
  #Run SEIR model
  predictions <- SEIRsim(xi, 
                         In1, In2, In345, In6, 
                         beta1, beta2, beta3, beta4, beta5, beta6
                         )
  
  ## Optimization of the peak of the epidemic
  #Calculate peak time in data
  peakTime <- data %>% 
    group_by(ageGroup) %>% 
    summarise(maxConfir = max(confirmed), maxConfiDate = startDate[which.max(confirmed)]) %>% 
    mutate(daysAfterStart = maxConfiDate - as.Date("2023-09-18")) %>% mutate(daysAfterStart = as.double(daysAfterStart)) %>% 
    select(daysAfterStart) %>% 
    pull()/7
  
  # Combine infection data for groups (I1, I2, I345, I678, I9)
  I1comb <- predictions$I21
  I2comb <- predictions$I22
  I345comb <- predictions$I23 + predictions$I24 + predictions$I25
  I6comb <- predictions$I26 
  
  
  # Combine the daily data into weekly totals for each group
  I1combWeek <- sapply(seq(1, length(I1comb), by = 7), function(i) sum(I1comb[i:(i+6)]))
  I2combWeek <- sapply(seq(1, length(I2comb), by = 7), function(i) sum(I2comb[i:(i+6)]))
  I345combWeek <- sapply(seq(1, length(I345comb), by = 7), function(i) sum(I345comb[i:(i+6)]))
  I6combWeek <- sapply(seq(1, length(I6comb), by = 7), function(i) sum(I6comb[i:(i+6)]))
  
  # Find the peak time from the simulation for each group
  I1peak <- which.max(I1combWeek)
  I2peak <- which.max(I2combWeek)
  I345peak <- which.max(I345combWeek)
  I6peak <- which.max(I6combWeek)
  
  # Calculate squared error between simulated peak time and actual peak from data
  peakPenalty1 <- (I1peak - peakTime[1])^2
  peakPenalty2 <- (I2peak - peakTime[2])^2
  peakPenalty345 <- (I345peak - peakTime[3])^2
  peakPenalty6 <- (I6peak - peakTime[4])^2
  #print("peakp")
  #print(peakPenalty7)
  
  #Normalization so values range between 0 and 1
  peakPenaltyMax <- 400
  
  normPeakPenalty1 <- peakPenalty1 / peakPenaltyMax
  normPeakPenalty2 <- peakPenalty2 / peakPenaltyMax
  normPeakPenalty345 <- peakPenalty345 / peakPenaltyMax
  normPeakPenalty6 <- peakPenalty6 / peakPenaltyMax
  #print("peaknp")
  #print(normPeakPenalty7)
  
  ## Optimization of ascertainment percent for the 9 groups
  # Calculate total number of individuals infected during the model period
  inf1tot <- last(predictions$C1)
  inf2tot <- last(predictions$C2)
  inf345tot <- last(predictions$C3 + predictions$C4 + predictions$C5)
  inf6tot <- last(predictions$C6)
  
  # Extract from data how many were infected during the period for each group
  infTotData <- data %>% 
    filter(week == "2024-U20") %>% 
    select(ageGroup, cumConfirmed) %>% 
    select(cumConfirmed) %>% 
    pull()
  
  # Calculate squared error between ascertainment percent * simulated infections and actual data
  inf1penalty <- (inf1tot * perc1 - infTotData[1])^2
  inf2penalty <- (inf2tot * perc2 - infTotData[2])^2
  inf345penalty <- (inf345tot * perc345 - infTotData[3])^2
  inf6penalty <- (inf6tot * perc6 - infTotData[4])^2
  
  # Normalize infection penalties
  inf1penaltyMax <- (pop$Number[1] * 0.2 - infTotData[1])^2
  inf2penaltyMax <- (pop$Number[2] * 0.2 - infTotData[2])^2
  inf345penaltyMax <- ((pop$Number[3]+pop$Number[4]+ pop$Number[5]) * 0.2 - infTotData[3])^2
  inf6penaltyMax <- (pop$Number[6] * 0.2 - infTotData[4])^2
  
  normInf1penalty <- inf1penalty / inf1penaltyMax
  normInf2penalty <- inf2penalty / inf2penaltyMax
  normInf345penalty <- inf345penalty / inf345penaltyMax
  normInf6penalty <- inf6penalty / inf6penaltyMax
  
  #print("infpenN")
  #print(normInf7penalty)
  
  # Return the sum of all penalties as the objective value
  totalPenalty <- sum(normInf1penalty, normInf2penalty, normInf345penalty, normInf6penalty, 
                      normPeakPenalty1, normPeakPenalty2, normPeakPenalty345, normPeakPenalty6)#,
                      
  print(totalPenalty)
  
  #print(c(normPeakPenalty1, normPeakPenalty2, normPeakPenalty3, normPeakPenalty4, normPeakPenalty5, normPeakPenalty6, normPeakPenalty7, normPeakPenalty8, normPeakPenalty9))
  #print(c(normInf1penalty, normInf2penalty, normInf3penalty, normInf4penalty, normInf5penalty, normInf6penalty, normInf7penalty, normInf8penalty, normInf9penalty))
  return(totalPenalty)
}


# Objective function wrapper for optimization with the new 5 groups (I1, I2, I345, I678, I9)
objFuncOptim <- function(parameters) {
  # Printing parameters to see if they're correctly assigned
  objFunc(
    xi = parameters[1],
    In1 = parameters[2], 
    In2 = parameters[3], 
    In345 = parameters[4], 
    In6 = parameters[5],
    beta1 = parameters[6],
    beta2 = parameters[7],
    beta3 = parameters[8],
    beta4 = parameters[9],
    beta5 = parameters[10],
    beta6 = parameters[11],
    perc1 = parameters[12],
    perc2 = parameters[13],
    perc345 = parameters[14],
    perc6 = parameters[15])
}


#Starting values, lower and upper limits

startingVal <-  c(0.49, #xi
                  75, 100, 150, 100,  #In
                  0.998, 0.4, 0.1, 0.1, 0.1, 0.1, #beta relative suseptibility
                  0.1, 0.05, 0.05, 0.05) #percent that goes to doctor


lower <- c(0.1, #xi
           10, 10, 10, 10,  #Initial infected
           0.5, 0.01, 0.01, 0.01, #beta
           0.01, 0.01, 
           0.02, 0.02, 0.001, 0.02 #percent that goes to doctor
)

upper <- c(0.9, #xi
           150, 150, 250, 150, #Initial infected
           1, 0.9, 0.3, 0.2, #beta
           0.2, 0.3, 
           0.15, 0.1, 0.1, 0.1 #percent that goes to doctor
)

#Optimization procedure
startTime <- Sys.time()
control_list <- list(maxfeval = 10000, tol = 1e-6, trace = 1)

optim <- nmkb(par = startingVal, fn = objFuncOptim,
              lower = lower,
              upper = upper,
              control = control_list)

endTime <- Sys.time()
endTime - startTime
print(optim)

#Save optimized parameters
saveRDS(optim, file = "Output/rsvModelOptim2.rds")

# Result using optimized parameter values
sol <- SEIRsim(
  xi = optim$par[1], 
  In1 = round(optim$par[2], 0), In2 = round(optim$par[3], 0), In345 = round(optim$par[4], 0),In6 = round(optim$par[5], 0),
  beta1 = optim$par[6], beta2 = optim$par[7], beta3 = optim$par[8], beta4 = optim$par[9],
  beta5 = optim$par[10], beta6 = optim$par[11])

# Extract ascertainment probabilities
perc <- c(optim$par[12], optim$par[13], optim$par[14], optim$par[15])


#Save ascertainment probabilities
saveRDS(perc, file = "Output/rsvModelPerc2.rds")

# Calculate daily new infections for each age group
new_infections <- sol %>%
  mutate(across(starts_with("C"), ~ c(NA, diff(.x)), .names = "New{col}"))

# Replace NA with 0 (for the first row where no previous day exists)
new_infections[is.na(new_infections)] <- 0
sol <- new_infections

#Put into tibble
solution <- tibble(time = seq(as.Date("2023-09-18"), as.Date("2024-05-19"), by = "1 day"))

for (i in 2:ncol(sol)){
  colName <- colnames(sol)[i]
  solution <- solution %>% mutate(!!colName := sol[,i])
}


#Create long data frame
sol <- solution %>% pivot_longer(!time, names_to = "state", values_to = "n") 

#Save solutions
saveRDS(sol, file = paste0("Output/rsvModelSol2.rds"))

