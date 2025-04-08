library(deSolve)

RSVequ <- function(t, state, param) {
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


SBIequ <- function(t, state, param) {

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
    
    return(list(c(dHRSV, dHSp, dHSa, dHMp, dR, dDRSV, dDSBI, dHRSVCumu, dHSpCumu, dHSaCumu, dHMpCumu)))
  })
}
