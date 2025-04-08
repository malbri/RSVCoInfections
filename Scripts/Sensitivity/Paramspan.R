rm(list = ls())
#packages
library(tidyverse)

#Load parameters
optimRSV <- readRDS(file = "Output/rsvModelOptim2.rds" )
optimSBI <- readRDS(file = "Output/SBIoptim.rds")

#Parameters that needs to be included

In <- round(c(In1 = round(optimRSV$par[2], 0), In2 = round(optimRSV$par[3], 0), In345 = round(optimRSV$par[4], 0),In6 = round(optimRSV$par[5], 0)))

xi <- optimRSV$par[1]

beta <- c(beta1 = optimRSV$par[6], beta2 = optimRSV$par[7], beta3 = optimRSV$par[8], beta4 = optimRSV$par[9], beta5 = optimRSV$par[10], beta6 = optimRSV$par[11]) 
  
rho <- c(0.0916, 0.163, 0.516, 0.753, 0.753, 0.753)

sigma <- 0.5
gamma <- 0.2
alpha <- 0.634


#From SBI model
phi <- c(phi1 = optimSBI$par[1], phi2 = optimSBI$par[2], phi34 = optimSBI$par[3], phi5 = optimSBI$par[4], phi6 = optimSBI$par[5])
mu <- c(mu12 = optimSBI$par[6], mu3 =optimSBI$par[7] , mu4 = optimSBI$par[8], mu5 = optimSBI$par[9], mu6 = optimSBI$par[10])

epsilonSBI = c(epsilonSBI1 = 1, epsilonSBI2 = 1, epsilonSBI3 = 1, 
               epsilonSBI4 = 1, epsilonSBI5 = 2, epsilonSBI6 = 2)

gammaRSV <- c(gammaRSV1 = 1/5, gammaRSV2 = 1/5, gammaRSV3 = 1/5,
              gammaRSV4 = 1/5, gammaRSV5 = 1/5, gammaRSV6 = 1/5)

gammaSBI <- c(gammaSBI1 = 1/(5*1.16), gammaSBI2 = 1/(5*1.16),  gammaSBI3 = 1/(5*1.16),
              gammaSBI4 = 1/(5*1.16), gammaSBI5 = 1/(5*1.16), gammaSBI6 = 1/(5*1.16))

# Draw 500 samples for all parameters with ±20% variability

# 1. Initial conditions (In)
In1 <- round(runif(500, min = In[1] - In[1] * 0.2, max = In[1] + In[1] * 0.2))
In2 <- round(runif(500, min = In[2] - In[2] * 0.2, max = In[2] + In[2] * 0.2))
In345 <- round(runif(500, min = In[3] - In[3] * 0.2, max = In[3] + In[3] * 0.2))
In6 <- round(runif(500, min = In[4] - In[4] * 0.2, max = In[4] + In[4] * 0.2))

#For increased risk of death due to SBI for the 4 youngest age groups
epsilonSBI1 <- runif(500, min = epsilonSBI[1], max = epsilonSBI[1] + epsilonSBI[1]*0.2)
epsilonSBI2 <- runif(500, min = epsilonSBI[2], max = epsilonSBI[2] + epsilonSBI[2]*0.2)
epsilonSBI3 <- runif(500, min = epsilonSBI[3], max = epsilonSBI[3] + epsilonSBI[3]*0.2)
epsilonSBI4 <- runif(500, min = epsilonSBI[4], max = epsilonSBI[4] + epsilonSBI[4]*0.2)


#Create dataframe for parameters, and add initial conditions
paramSpan <- tibble(In1, In2, In345, In6, epsilonSBI1, epsilonSBI2, epsilonSBI3, epsilonSBI4)

#Add all the remaining parameters to 1 vector
paramList <- c(xi=xi, beta, rho = rho, sigma = sigma, gamma = gamma, alpha = alpha, phi, gammaRSV, gammaSBI, mu , epsilonSBI[4:6])

#Loop to extract 500 parameter spans of +-20%, add to dataframe
for (i in 1:length(paramList)){
  tempVar <- paramList[i]
  tempVec <- runif(500, min = tempVar-tempVar*0.2, max = tempVar + tempVar*0.2)
  paramSpan <- paramSpan %>% mutate(!!names(paramList[i]) := tempVec)
}

#Sort columns
paramSpan <- paramSpan %>% select(sort(tidyselect::peek_vars()))

#Save param span matrix
saveRDS(paramSpan, file = "Output/paramSpan.rds")

