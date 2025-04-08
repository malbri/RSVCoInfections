library(tidyverse)

#Load contact matrix
c <- readRDS(file = "Data/contactmatrix.rds")

###Population###
#load population
pop <- read_xlsx("Data/Population.xlsx")

#Total population size
N <- sum(pop$Number)

# the parameters values:
RSVparamVal <- list(
  #natural maternal immunity rate
  xi = paramSpan$xi[runN],
  #relative susceptibility
  beta = c(paramSpan$beta1[runN], paramSpan$beta2[runN], paramSpan$beta3[runN],paramSpan$beta4[runN], paramSpan$beta5[runN],  paramSpan$beta6[runN]),
  
  #Contracts
  c = c,
  #Latency rate 
  sigma = paramSpan$sigma[runN],
  #Infectious period
  gamma = paramSpan$gamma[runN],
  
  #proportion asymptomatic
  rho = c(paramSpan$rho1[runN],paramSpan$rho2[runN],paramSpan$rho3[runN],paramSpan$rho4[runN],paramSpan$rho5[runN],paramSpan$rho6[runN]),
  
  #smitte asymptomatic
  alpha = c(rep(paramSpan$alpha[runN], 6))
)


# the initial values of variables:
RSVinit <- c(
  S = pop$Number - c(paramSpan$In1[runN], paramSpan$In2[runN], paramSpan$In345[runN], paramSpan$In345[runN],paramSpan$In345[runN], paramSpan$In6[runN]),
  E1 = rep(0,6),
  E2 = rep(0,6),
  IA = c(0.0916*paramSpan$In1[runN], 0.163*paramSpan$In2[runN], 0.516*paramSpan$In345[runN], 0.753*paramSpan$In345[runN], 0.753*paramSpan$In345[runN], 0.753*paramSpan$In6[runN]),
  I1 = c((1-0.0916)*paramSpan$In1[runN], (1-0.163)*paramSpan$In2[runN], (1-0.516)*paramSpan$In345[runN], (1-0.753)*paramSpan$In345[runN], (1-0.753)*paramSpan$In345[runN], (1-0.753)*paramSpan$In6[runN]),
  I2 = rep(0,6),
  R = rep(0,6),
  C= rep(0,6))


#u0, states and initial conditions
#RSVinit <- data.frame(S1 = rep(pop$Number[1]-paramSpan$In1[runN]), 
                # S2 = pop$Number[2]-paramSpan$In2[runN], 
#                 S3 = pop$Number[3]-paramSpan$In3[runN], 
 #                S4 = pop$Number[4]-paramSpan$In4[runN], 
  #               S5 = pop$Number[5]-paramSpan$In5[runN],
   #              S6 = pop$Number[6]-paramSpan$In5[runN],
    #             E11 = 0,E12 = 0,E13 = 0,E14 = 0,E15 = 0,E16 = 0,
     #            E21 = 0,E22 = 0,E23 = 0,E24 = 0,E25 = 0,E26 = 0,
      #           I21 = (1-0.0916)*paramSpan$In1[runN], 
       #          I22 = (1-0.163)*paramSpan$In2[runN], 
        #         I23 = (1-0.516)*paramSpan$In3[runN], 
         #        I24 = (1-0.753)*paramSpan$In4[runN], 
          #       I25 = (1-0.753)*paramSpan$In5[runN], 
           #      I26 = (1-0.753)*paramSpan$In5[runN],
            #     I21 = 0, I22 = 0, I23 = 0, I24 = 0, I25 = 0, I26 = 0,
             #    IA1 = 0.0916*paramSpan$In1[runN], 
              #   IA2 = 0.163*paramSpan$In2[runN], 
               #  IA3 = 0.516*paramSpan$In3[runN], 
                # IA4 = 0.753*paramSpan$In4[runN], 
                 #IA5 = 0.753*paramSpan$In5[runN], 
#                 IA6 = 0.753*paramSpan$In5[runN],
 #                R1 = 0, R2 = 0, R3 = 0, R4 = 0, R5 = 0,
#)