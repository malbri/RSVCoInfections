rm(list = ls())

# Define age-specific co-infection rates
co_infection_rate_123 <- 0.324  # 32.4% for age groups 1-6
co_infection_rate_456 <- 0.266   # 26.6% for age groups 7-9

etaRSV <- c(1 - co_infection_rate_123,1 - co_infection_rate_123,1 - co_infection_rate_123, 1 - co_infection_rate_456, 1 - co_infection_rate_456, 1 - co_infection_rate_456)


# Under 18 (<18): Mycoplasma pneumoniae = 0.6%, Streptococcus pneumoniae = 13.2%, Staphylococcus aureus = 13.2%
# Age 18+ (7-9): Mycoplasma pneumoniae = 14.1%, Streptococcus pneumoniae = 23.5%, Staphylococcus aureus = 10.6%

# Calculate co-infections for age groups 1-6
etaMp_123 <- (co_infection_rate_123)* 0.006
etaSp_123 <- (co_infection_rate_123)* 0.132
etaSa_123 <- (co_infection_rate_123)* 0.132

# Calculate co-infections for age groups 7-9
etaMp_456 <- (co_infection_rate_456) * 0.0705
etaSp_456 <- (co_infection_rate_456) * 0.4225
etaSa_456 <- (co_infection_rate_456) * 0.088

#ensure sum of eta = 1
etatotal_123 <- etaMp_123 + etaSa_123 + etaSp_123 + etaRSV[1]
etaRSV[1:3] <- etaRSV[1] / sum(etatotal_123)
etaMp_123 <- etaMp_123 / sum(etatotal_123)
etaSp_123 <- etaSp_123 / sum(etatotal_123)
etaSa_123 <- etaSa_123 / sum(etatotal_123)

etatotal_456 <- etaMp_456 + etaSa_456 + etaSp_456 + etaRSV[2]
etaRSV[4:6] <- etaRSV[2] / sum(etatotal_456)
etaMp_456 <- etaMp_456 / sum(etatotal_456)
etaSp_456 <- etaSp_456 / sum(etatotal_456)
etaSa_456 <- etaSa_456 / sum(etatotal_456)

etaMp <- c(etaMp_123,etaMp_123,etaMp_123,etaMp_456,etaMp_456, etaMp_456)
etaSp <- c(etaSp_123,etaSp_123,etaSp_123,etaSp_456,etaSp_456,etaSp_456)
etaSa <- c(etaSa_123,etaSa_123,etaSa_123,etaSa_456,etaSa_456,etaSa_456)

saveRDS(etaRSV,file = "Data/04EtaRSV.rds")
saveRDS(etaMp,file = "Data/04EtaMp.rds")
saveRDS(etaSa,file = "Data/04EtaSa.rds")
saveRDS(etaSp,file = "Data/04EtaSp.rds")
