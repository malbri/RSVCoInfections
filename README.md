# 📘 Modelling Respiratory Syncytial Virus (RSV) and Bacterial Co-infections in Denmark
This repository contains code and data for modeling **Respiratory Syncytial Virus (RSV)** dynamics and its interactions with co-infecting pathogens using a compartmental model implemented in R.

---

## 📁 Folder Structure

- **`Data/`**  

Contains input datasets used for simulation and model fitting, including infection incidence, population demographics, and co-infection rates. Raw data is obtained from SSI RSV dashbord available from https://experience.arcgis.com/experience/220fef27d07d438889d651cc2e00076c/page/RS-virus/. 

Following Excel file is used from the downloaded data: 02_rsv_epikurve_season_region_uge_agegrp

- **`Scripts/`**  
  Core model code, simulations, and analysis scripts. Key files include:
  
  - `01aCleanCasesAll.R` – Preprocess and clean case data  
  - `01aContactMatrix.R` – Generate or load contact matrix  
  - `01bContactMatrixPlot.R` – Visualize contact patterns  
  - `02aOptimRSV_SEIAIR.R` – Calibrate RSV SEIAIR model  
  - `02bValidationPlotRSV.R` – Validate model against observed RSV data  
  - `03InputToSBIModel.R` – Prepare inputs for the SBI (Hospital) model  
  - `04aEta.R` – Calculate eta parameter (percentage with bacterial co-infections)  
  - `04bOptimSBI.R` – Optimize parameters for hospital model 
  - `04cValidationPlotsSBI.R` – Validation plots for the hospital model  

  Subfolders:
  - **`Scenarios/`** – Scenario-based simulations and projections  
  - **`Sensitivity/`** – Sensitivity analyses and parameter sweeps

- **`Output/`**  
  Contains all simulation results and processed model outputs.

- **`Plots/`**  
  Final figures and visualization outputs.

Maja L. Brinch. (2025). Modelling Respiratory Syncytial Virus (RSV) and Bacterial Co-infections in Denmark - Repository. https://github.com/malbri/rsv-co-infection
