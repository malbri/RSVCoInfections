# the parameters values:
SBIparamVal <- list(
  #Hospitalization rate
  phi = c(paramSpan$phi1[runN],paramSpan$phi2[runN],paramSpan$phi34[runN],paramSpan$phi34[runN],
          paramSpan$phi5[runN],paramSpan$phi6[runN]),
  
  
  #Recovery rate from RSV hospitalization
  gammaRSV = c(paramSpan$gammaRSV1[runN],paramSpan$gammaRSV2[runN],paramSpan$gammaRSV3[runN],
                 paramSpan$gammaRSV4[runN],paramSpan$gammaRSV5[runN],paramSpan$gammaRSV6[runN]), 
  
  #Recovery rate from RSV and SBI hospitalizations
  gammaSBI = c(paramSpan$gammaSBI1[runN],paramSpan$gammaSBI2[runN],paramSpan$gammaSBI3[runN],
                paramSpan$gammaSBI4[runN],paramSpan$gammaSBI5[runN],paramSpan$gammaSBI6[runN]), 
  
  #Death rate
  mu = c(paramSpan$mu12[runN],paramSpan$mu12[runN],paramSpan$mu3[runN],
         paramSpan$mu4[runN],paramSpan$mu5[runN],paramSpan$mu6[runN]),
  
  #factor up scaling deaths are due to RSV + SBI hospitalizations
  epsilonSBI = c(paramSpan$epsilonSBI1[runN],paramSpan$epsilonSBI2[runN],paramSpan$epsilonSBI3[runN],
                 paramSpan$epsilonSBI4[runN],paramSpan$epsilonSBI5[runN],paramSpan$epsilonSBI6[runN])
)

SBIinit <- c(
  HRSV = rep(0, 6),  
  HSp = rep(0, 6),
  HSa = rep(0, 6),
  HMp = rep(0, 6),
  R = rep(0, 6),
  DRSV = rep(0, 6),
  DSBI = rep(0, 6),
  HRSVCumu = rep(0, 6),
  HSpCumu = rep(0, 6),
  HSaCumu = rep(0, 6),
  HMpCumu = rep(0, 6)
)

