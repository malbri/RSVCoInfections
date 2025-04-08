rm(list = ls())

library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyverse)

results_RSV <- readRDS("Output/result_RSV.rds")
results_HOS <- readRDS("Output/result_HOS.rds")

strategies <- c("baseline","1","2","3","4","5","6","7","8","9","10","11","12","13","14")
strategies <- data.frame(strategy = strategies)
results <- results_HOS %>% add_column(results_RSV)
results <- strategies %>% add_column(results)


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

results <- results %>% mutate(across(where(is.numeric), round))

# Initialize baseline values for each age group
baseline_RSV <- results %>% select(starts_with("C")) %>% slice(1)
baseline_RSVhos <- results %>% select(starts_with("HRSVCumu")) %>% slice(1)
baseline_RSVdeath <- results %>% select(starts_with("DRSV")) %>% slice(1)

# Loop through each age group and calculate differences and percentage differences
for (i in 1:6) {
  results <- results %>%
    mutate(
      !!paste0("difference_RSV", i) := round(!!sym(paste0("C", i)) - baseline_RSV[[paste0("C", i)]]),
      !!paste0("pct_diff_RSV", i) := round((!!sym(paste0("difference_RSV", i)) / baseline_RSV[[paste0("C", i)]]) * 100),
      
      !!paste0("difference_RSVhos", i) := round(!!sym(paste0("HRSVCumu", i)) - baseline_RSVhos[[paste0("HRSVCumu", i)]]),
      !!paste0("pct_diff_RSVhos", i) := round((!!sym(paste0("difference_RSVhos", i)) / baseline_RSVhos[[paste0("HRSVCumu", i)]]) * 100),
      
      !!paste0("difference_RSVdeath", i) := round(!!sym(paste0("DRSV", i)) - baseline_RSVdeath[[paste0("DRSV", i)]]),
      !!paste0("pct_diff_RSVdeath", i) := round((!!sym(paste0("difference_RSVdeath", i)) / baseline_RSVdeath[[paste0("DRSV", i)]]) * 100)
    )
}



# Function to create plots for each age group
create_plots <- function(age_group) {
  # Plot for nRSV
  RSVplot <- ggplot(results, aes(y = StratPlot, x = !!sym(paste0("C", age_group)))) +
    geom_bar(stat = "identity", fill = "cadetblue") +
    labs(
      title = paste("Total number of RSV cases"),
      y = NULL,
      x = "Number of RSV Cases",
      fill = "StratPlot"
    ) +
    theme_minimal() +
    geom_text(aes(label = ifelse(strategy == "baseline",
                                 ceiling(!!sym(paste0("C", age_group))),
                                 paste0(ceiling(!!sym(paste0("C", age_group))), " (", round(!!sym(paste0("pct_diff_RSV", age_group)), 0), "%)" ))), 
              position = position_stack(vjust = 0.5), 
              color = "black",
              size = 4) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
          axis.text = element_text(size = 12))
  
  # Plot for nHospRSV
  RSVHosplot <- ggplot(results, aes(y = StratPlot, x = !!sym(paste0("HRSVCumu", age_group)))) +
    geom_bar(stat = "identity", fill = "cadetblue") +
    labs(
      title = paste("Total hospitalizations due to RSV"),
      y = NULL,
      x = "Number of Hospitalizations",
      fill = "StratPlot"
    ) +
    theme_minimal() +
    geom_text(aes(label = ifelse(strategy == "baseline",
                                 ceiling(!!sym(paste0("HRSVCumu", age_group))),
                                 paste0(ceiling(!!sym(paste0("HRSVCumu", age_group))), " (", round(!!sym(paste0("pct_diff_RSVhos", age_group)), 0), "%)" ))), 
              position = position_stack(vjust = 0.5), 
              color = "black",
              size = 4) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
          axis.text.y = element_blank(),
          axis.text = element_text(size = 12))
  
  # Plot for nDeathsRSV
  RSVdeathplot <- ggplot(results, aes(y = StratPlot, x = !!sym(paste0("DRSV", age_group)))) +
    geom_bar(stat = "identity", fill = "cadetblue") +
    labs(
      title = paste("Total Deaths Due to RSV"),
      y = NULL,
      x = "Number of Deaths",
      fill = "StratPlot"
    ) +
    theme_minimal() +
    geom_text(aes(label = ifelse(strategy == "baseline",
                                 ceiling(!!sym(paste0("DRSV", age_group))),
                                 paste0(ceiling(!!sym(paste0("DRSV", age_group))), " (", round(!!sym(paste0("pct_diff_RSVdeath", age_group)), 0), "%)" ))), 
              position = position_stack(vjust = 0.5), 
              color = "black",
              size = 4) +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, margin = margin(t = 10, b = 10)),
          axis.text.y = element_blank(),
          axis.text = element_text(size = 12))
  
  list(RSVplot, RSVHosplot, RSVdeathplot)
}

# Create and save plots for each age group
for (age_group in 1:6) {
  plots <- create_plots(age_group)
  PlotSupp <- plots[[1]] + plots[[2]] + plots[[3]] +
    plot_annotation(
      title = paste("Comparison of RSV strategies for Age Group", age_group),
      theme = theme(plot.title = element_text(size = 20, hjust = 0.5))
    )
  ggsave(PlotSupp, filename = paste0("Plots/Strategies_RSV_AgeGroup", age_group, ".png"), width = 20, height = 10)
}
