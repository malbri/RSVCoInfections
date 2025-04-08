rm(list = ls())

#load packages
library(epiR)
library(tidyverse)
library(RColorBrewer)
library(Hmisc)
library(ggcorrplot)
library(GGally)
library(xtable)

#Colors
colors <- brewer.pal(12, "Paired")[c(2,4,6,8,10,12)]

#Load parameters + outcomes
paramOutcomes <- readRDS(file = "Output/paramOutcomes.rds")

#Linear correlations
results <- as.data.frame(paramOutcomes)
corrData <- rcorr(as.matrix(results), type = "pearson")

#Between paramters
corrDataParam <- corrData$r[1:48,1:48]
corrDataParamPval <- corrData$P[1:48,1:48]

#Remove diagonal
diag(corrDataParam) <- NA

#Create plot of linear correlations between parameters
corrParamPlot <- ggcorrplot(corrDataParam, type = "lower",title = "Linear correlations between parameters",
           lab = FALSE, method = "square", p.mat = corrDataParamPval,
           sig.level = 0.05, pch.cex = 2,tl.cex = 9,tl.col = "black", tl.srt = 90)
print(corrParamPlot)
#ggsave(corrParamPlot, file = "output/07_sensitivityAnalysis/corrParamPlot.png", height = 10, width = 12,bg = "white")

#Function to flatten the correlation matrix
#Source: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corrDataParamFlat <- flattenCorrMatrix(corrDataParam,corrDataParamPval)

#Significant correlations between parameters
corrSignParamInt <- corrDataParamFlat %>% filter(p < 0.05)

#Find maximal (and minimal) correlation between the significant correlations
corrDataParamFlat %>% filter(p < 0.05) %>% slice(which.max(cor))

corrDataParamFlat %>% filter(p < 0.05) %>% slice(which.min(cor))

#Split into 2 data frames
splitCorrData <- split(corrSignParamInt, factor(sort(rank(row.names(corrSignParamInt))%%2)))

#Create latex table for thesis
xtable(splitCorrData$"0")
xtable(splitCorrData$"1")

#Between significant parameters and outcomes
corrData <- flattenCorrMatrix(corrData$r,corrData$P)

#Find all parameters with a significant correlation to number of SBI hosp or deaths
signCorrParamOutcomes <- corrData %>% filter(p < 0.05, column %in% c("nHospSBI","nDeathsSBI")) %>% select(row) %>% pull()
HsignCorrParamOutcomes <- corrData %>% filter(p < 0.05, column %in% c("nHospSBI")) %>% select(row) %>% pull()
DsignCorrParamOutcomes <- corrData %>% filter(p < 0.05, column %in% c("nDeathsSBI")) %>% select(row) %>% pull()


#Extract the data for the significant parameters
signCorrData <- results %>% select(all_of(signCorrParamOutcomes),nHospSBI,nDeathsSBI)
names(signCorrData)

#2 plots with the significant parameters against the 2 outcomes
linCorPlot1 <- ggpairs(signCorrData[c(1:3,5,10:13,17,18)],
                       diag = list(continuous = "blankDiag"),
                       columnLabels = c("alpha",  "beta[4]", "beta[6]", "gamma", "rho[2]" , "rho[4]", "rho[5]", "rho[6]",
                                        "nHospCO","nDeathsCO"), labeller = label_parsed)+
  theme_bw(base_size = 11) +
  labs(title = "A. RSV model parameters")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

linCorPlot2 <- ggpairs(signCorrData[c(4,6:9,14:18)],
                       diag = list(continuous = "blankDiag"),
                       columnLabels = c("epsilonCO[3]","gammaCO[4]", "gammaCO[6]", "phi[1]", "phi[2]", 
                                        "epsilonCO[6]", "mu[6]", "phi[6]",
                                        "nHospCO","nDeathsCO"), labeller = label_parsed)+
  theme_bw(base_size = 11) +
  labs(title = "B. Hospital model parameters")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(linCorPlot1, file = "Plots/linCorPlot1.png", width = 9.5, height = 9)
ggsave(linCorPlot2, file = "Plots/linCorPlot2.png", width = 9.5, height = 9)

####PRCC####
#Calculate PRCC between parameters and the 2 outcomes
prccHosp <- epi.prcc(paramOutcomes[1:49], sided.test = 2, conf.level = 0.95)
prccDeath <- epi.prcc(paramOutcomes[c(1:48,50)], sided.test = 2, conf.level = 0.95)

#Name of the significant parameters
prcchos <- prccHosp %>% filter(p.value < 0.05)
prccdeath <- prccDeath %>% filter(p.value < 0.05)

prcchos$var
prccdeath$var

#PRCC plots
prccHospPlot <- prccHosp %>% filter(p.value < 0.05) %>% 
  ggplot(aes(x = var, 
             y = est,
             fill = var)) +
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_label(aes(label=round(est,2)), colour = "white", fontface = "bold")+
  scale_fill_manual(values = c(colors[1],"#708090",colors[2],
                               colors[4],colors[6],colors[1],colors[6],
                               "#708090",colors[2],colors[5],colors[6],"#708090",
                               colors[2],colors[3],colors[4],
                               colors[5],colors[6], colors[1])) + 
  theme_bw(base_size = 12) + 
  labs(title = "A. Partial rank correlation coefficients - Total number of hospitalisations with co-infections",
       x = "",
       y = "PRCC") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14)) + 
  scale_x_discrete(limit = c("alpha", "beta1", "beta2", "beta4", "beta6", "gamma", 
                             "In6", "phi1", "phi2", "phi5", "phi6", 
                             "rho1", "rho2", "rho3", "rho4", "rho5", 
                             "rho6", "xi"),
                   labels = c(expression(alpha), expression(beta[1]), 
                              expression(beta[2]), expression(beta[4]), expression(beta[6]), 
                              expression(gamma), expression(In[6]), expression(phi[1]), 
                              expression(phi[2]), expression(phi[5]), expression(phi[6]), 
                              expression(rho[1]), expression(rho[2]), expression(rho[3]),
                              expression(rho[4]), expression(rho[5]), 
                              expression(rho[6]), expression(xi)))


ggsave(prccHospPlot, file = "Plots/prccHospPlot.png", height = 4.5, width = 10)

prccDeathPlot <- prccDeath %>% filter(p.value < 0.05) %>% 
  ggplot(aes(x = var, 
             y = est,
             fill = var)) +
  geom_bar(stat="identity", alpha = 0.6) + 
  geom_label(aes(label=round(est,2)), colour = "white", fontface = "bold") +
  scale_fill_manual(values = c(colors[1], colors[3], colors[4], colors[5],
                               colors[6], colors[3], colors[5], colors[6],
                               colors[1], colors[5], colors[3], colors[6],
                               colors[5], colors[6], colors[5], colors[6],
                               colors[3], colors[4], colors[5], colors[6], colors[1])) + 
  theme_bw(base_size = 12) + 
  labs(title = "B. Partial rank correlation coefficients - Total number of deaths with co-infections",
       x = "",
       y = "PRCC") +
  theme(legend.position = "none",
        axis.text = element_text(size = 14)) + 
  scale_x_discrete(limit = c("alpha", "beta3", "beta4", "beta5", "beta6",
                             "epsilonSBI3","epsilonSBI5", "epsilonSBI6", 
                             "gamma", "gammaRSV5", "gammaSBI3", "gammaSBI6", 
                             "mu5", "mu6", "phi5", "phi6", 
                             "rho3","rho4", "rho5", "rho6", "xi"),
                   labels = c(expression(alpha), 
                              expression(beta[3]),expression(beta[4]), expression(beta[5]), expression(beta[6]),
                              expression(epsilon[3]),expression(epsilon[5]), expression(epsilon[6]), 
                              expression(gamma), expression(gamma[5]^RSV), 
                              expression(gamma[3]^CO), expression(gamma[6]^CO), 
                              expression(mu[5]), expression(mu[6]), 
                              expression(phi[5]), expression(phi[6]), 
                              expression(rho[3]), expression(rho[4]),
                              expression(rho[5]), expression(rho[6]),
                              expression(xi)))


ggsave(prccDeathPlot, file = "Plots/prccDeathPlot.png", height = 4.5, width = 10)

