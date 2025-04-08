rm(list = ls())

#Load packages
library(tidyverse)
library(patchwork)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)

results <- readRDS("Output/OutcomesStrategies.rds")

#Make the vaccine strategy a factor
results$strategy <- factor(results$strategy)

#Add vaccine strategy description
results <- results %>% mutate(StratPlot = case_when(strategy == "baseline" ~ "Baseline",
                                                        strategy == "1" ~ "Vaccine (50% of 65+ years)",
                                                        strategy == "2" ~ "Vaccine (75% of 65+ years)",
                                                        strategy == "3" ~ "Vaccine (95% of 65+ years)",
                                                        strategy == "4" ~ "Maternal vaccine (10%)",
                                                        strategy == "5" ~ "Maternal vaccine (15%)",
                                                        strategy == "6" ~ "Maternal vaccine (25%)",
                                                        strategy == "7" ~ "Preventive mAbs (50% <1 year)",
                                                        strategy == "8" ~ "Preventive mAbs (75% <1 year)",
                                                        strategy == "9" ~ "Preventive mAbs (95% <1 year)",
                                                        strategy == "10" ~ "Preventive mAbs (25% of 45+ years)",
                                                        strategy == "11" ~ "Preventive mAbs (50% of 45+ years)",
                                                        strategy == "12" ~ "Preventive mAbs (75% of 45+ years)",
                                                        strategy == "13" ~ "mAbs treatment in ICU",
                                                        strategy == "14" ~ "Vaccine maternal and 65+ years",
                                                        ))

#Calculate the difference from the baseline for each scenario
# Calculate baseline values
baseline_hos <- results$nHospSBI[1]
baseline_death <- results$nDeathsSBI[1]
baseline_RSV <- results$nRSV[1]
baseline_RSVhos <- results$nHospRSV[1]
baseline_RSVdeath <- results$nDeathsRSV[1]
baseline_antibiotics <- results$antibiotics[1]

# Add differences and percentage differences for all variables
results <- results %>%
  mutate(
    # Hospitalizations (SBI)
    difference_hos = round(nHospSBI - baseline_hos),
    pct_diff_hos = round((difference_hos / baseline_hos) * 100),
    
    # Deaths (SBI)
    difference_death = round(nDeathsSBI - baseline_death),
    pct_diff_death = round((difference_death / baseline_death) * 100),
    
    # RSV - total
    difference_RSV = round(nRSV - baseline_RSV),
    pct_diff_RSV = round((difference_RSV / baseline_RSV) * 100),
    
    # RSV hospitalizations
    difference_RSVhos = round(nHospRSV - baseline_RSVhos),
    pct_diff_RSVhos = round((difference_RSVhos / baseline_RSVhos) * 100),
    
    # RSV deaths
    difference_RSVdeath = round(nDeathsRSV - baseline_RSVdeath),
    pct_diff_RSVdeath = round((difference_RSVdeath / baseline_RSVdeath) * 100),
    
    # Antibiotics
    difference_antibiotics = round(antibiotics - baseline_antibiotics),
    pct_diff_antibiotics = round((difference_antibiotics / baseline_antibiotics) * 100)
  )

library(forcats)

# Plot for nRSV
RSVplot <- ggplot(results, aes(y = StratPlot, x = nRSV)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Total number of RSV cases",
    y = NULL,
    x = "Number of RSV Cases",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(nRSV),
                               paste0(ceiling(nRSV), " (", round(pct_diff_RSV, 0), "%)" ))), 
            x=250000, 
            color = "black",
            size = 4) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.text = element_text(size = 12))

# Plot for nHospRSV
RSVHosplot <- ggplot(results, aes(y = StratPlot, x = nHospRSV)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Total hospitalizations due to RSV",
    y = NULL,
    x = "Number of Hospitalizations",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(nHospRSV),
                               paste0(ceiling(nHospRSV), " (", round(pct_diff_RSVhos, 0), "%)" ))), 
            x=500, 
            color = "black",
            size = 4) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12))

# Plot for nDeathsRSV
RSVdeathplot <- ggplot(results, aes(y = StratPlot, x = nDeathsRSV)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Total Deaths Due to RSV",
    y = NULL,
    x = "Number of Deaths",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(nDeathsRSV),
                               paste0(ceiling(nDeathsRSV), " (", round(pct_diff_RSVdeath, 0), "%)" ))), 
            x=10, 
            color = "black",
            size = 4) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.text.y = element_blank(),
        axis.text = element_text(size = 12))

PlotSupp <- RSVplot+RSVHosplot+RSVdeathplot +
  plot_annotation(
  title = paste("Comparison of RSV strategies (all age groups)"),
  theme = theme(plot.title = element_text(size = 20, hjust = 0.5))
)
ggsave(PlotSupp,filename = "Plots/Strategies_RSV.png", width = 20, height = 10)

################ Figure for manuscript ##############
# Plot for nHospSBI
Hosplot <- ggplot(results, aes(y = StratPlot, x = nHospSBI)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Total hospitalizations with co-infections",
    y = NULL,
    x = "Number of Hospitalizations",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(nHospSBI),
                               paste0(ceiling(nHospSBI), " (", round(pct_diff_hos, 0), "%)" ))), 
            x=70, 
            color = "black",
            size = 5) +
  theme(plot.title = element_text(size = 16, hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 14))


# Plot for nDeathsSBI
Deathplot <-ggplot(results, aes(y = StratPlot, x = nDeathsSBI)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Total deaths with co-infections",
    y= NULL,
    x = "Number of Deaths",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(nDeathsSBI),
                               paste0(ceiling(nDeathsSBI), " (", round(pct_diff_death, 0), "%)" ))), 
            x=4.5, 
            color = "black",
            size = 5)+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 14))

# Plot for antibiotics
antibioticsplot <- ggplot(results, aes(y = StratPlot, x = antibiotics)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  labs(
    title = "Hospitalization receiving antibiotics",
    y = NULL,
    x = "Number of hospitalized individuals \nreceiving antibiotics",
    fill = "StratPlot"
  ) +
  theme_minimal() +
  geom_text(aes(label = ifelse(strategy == "baseline",
                               ceiling(antibiotics),
                               paste0(ceiling(antibiotics), " (", round(pct_diff_antibiotics, 0), "%)" ))), 
            x=450, 
            color = "black",
            size = 5)+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 14))


Plot_manus <- Hosplot+Deathplot+antibioticsplot 
Plot_manus
ggsave(Plot_manus,filename = "Plots/Strategies_CO.png", width = 15, height = 8)
