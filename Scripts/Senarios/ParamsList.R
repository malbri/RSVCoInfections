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



#Add all the parameters to 1 vector
paramList <- c(In, xi=xi, beta, rho = rho, sigma = sigma, gamma = gamma, alpha = alpha, phi, gammaRSV, gammaSBI, mu , epsilonSBI)

scenarios <- matrix (rep(paramList,15), nrow = 15, byrow = TRUE)
colnames(scenarios) <- names(paramList)

# Modify specific parameters for each scenario
# Baseline
scenarios[1,]
# Vaccination 65+
scenarios[2, c("beta6", "phi6")] <- scenarios[2, c("beta6", "phi6")]*c(0.675, 0.55)
scenarios[3, c("beta6", "phi6")] <- scenarios[3, c("beta6", "phi6")]*c(0.5125, 0.475)
scenarios[4, c("beta6", "phi6")] <- scenarios[4, c("beta6", "phi6")]*c(0.3825, 0.335)
# Vaccination maternal
scenarios[5, c("beta1", "phi1")] <- scenarios[5, c("beta1", "phi1")]*c(0.93, 0.945)
scenarios[6, c("beta1", "phi1")] <- scenarios[6, c("beta1", "phi1")]*c(0.895, 0.9175)
scenarios[7, c("beta1", "phi1")] <- scenarios[7, c("beta1", "phi1")]*c(0.825, 0.8625)
# mAbs infants
scenarios[8, c("beta1", "phi1")] <- scenarios[8, c("beta1", "phi1")]*c(0.6, 0.575)
scenarios[9, c("beta1", "phi1")] <- scenarios[9, c("beta1", "phi1")]*c(0.4, 0.3625)
scenarios[10, c("beta1", "phi1")] <- scenarios[10, c("beta1", "phi1")]*c(0.24, 0.1925)
# mAbs 65+ death
scenarios[11, c("mu5", "mu6")] <- scenarios[11, c("mu5", "mu6")]*c(0.85, 0.85)
scenarios[12, c("mu5", "mu6")] <- scenarios[12, c("mu5", "mu6")]*c(0.7, 0.7)
scenarios[13, c("mu5", "mu6")] <- scenarios[13, c("mu5", "mu6")]*c(0.55, 0.55)
# mAbs treatments ICU
scenarios[14, c("gammaRSV1", "gammaRSV2", "gammaRSV3", "gammaRSV4", "gammaRSV5", "gammaRSV6", 
                "gammaSBI1", "gammaSBI2", "gammaSBI3", "gammaSBI4", "gammaSBI5", "gammaSBI6")] <- 
  scenarios[14, c("gammaRSV1", "gammaRSV2", "gammaRSV3", "gammaRSV4", "gammaRSV5", "gammaRSV6", 
                  "gammaSBI1", "gammaSBI2", "gammaSBI3", "gammaSBI4", "gammaSBI5", "gammaSBI6")] * 
  c(1.068, 1.068, 1.068, 1.068, 1.068, 1.068, 1.187, 1.187, 1.187, 1.187, 1.187, 1.187)
#maternal and elderly vaccination
scenarios[15, c("beta1", "phi1","beta6", "phi6")] <- scenarios[15, c("beta1", "phi1","beta6", "phi6")]*c(0.825, 0.8625,0.5125, 0.475)

scenarios <- as.data.frame(scenarios)

#Save param span matrix
saveRDS(scenarios, file = "Output/Scenarios.rds")
