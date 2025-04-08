rm(list = ls())

#Load packages
library(tidyverse)
library(readxl)
library(hhh4contacts)

#Read contact matrix for the Danish population
cmDK <- read_excel(path = "Data/contactMatrix.xlsx", sheet = "Denmark")
cmDK <- as.matrix(cmDK)

#Give column and row names
colnames(cmDK) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                     "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                     "70-74", "75-80")
rownames(cmDK) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                    "70-74", "75-80")


#The data is in 5 year intervals, aggregate to age groups close to the ones in the models
cmDKagg <- aggregateC(as.matrix(cmDK), grouping = list("0-4" = c("0-4"), 
                                                       "5-14" = c("5-9","10-14"),
                                                       "15-44" = c("15-19","20-24","25-29","30-34","35-39","40-44"),
                                                       "45-64" = c("45-49", "50-54","55-59","60-64"),
                                                       "64+" = c("65-69","70-74","75-80")))

#Rearrange columns
Contact <- cmDKagg[c("0-4","5-14","15-44","45-64","64+"),c("0-4","5-14","15-44","45-64","64+")]

#Split youngest agegroup
#### Creating the population contact matrix ####
Population_contact <- matrix(nrow=6, ncol=6)

colnames(Population_contact) <- c("<1 år", "1-4 år", "5-14 år", "15-44 år", "45-64", "64+")
rownames(Population_contact) <- c("<1 år", "1-4 år", "5-14 år", "15-44 år", "45-64", "64+")

# Calculating the number of individuals in the different age groups 
Pop_str <- matrix(nrow=1, ncol=2)
colnames(Pop_str) <- c("<1 år", "1-4 år")
Pop_str[1,] <- c(0.1559, 0.8441)

# Creating diagonal in the population contact matrix
Population_contact[1,1] <- Contact[1,1]*Pop_str[1,1]
Population_contact[2,2] <- Contact[1,1]*Pop_str[1,2]

Population_contact[3,3] <- Contact[2,2]
Population_contact[4,4] <- Contact[3,3]
Population_contact[5,5] <- Contact[4,4]
Population_contact[6,6] <- Contact[5,5]

# Adding the information from the known DK contact matrix
Population_contact[3,4] <- Contact[2,3]
Population_contact[3,5] <- Contact[2,4]
Population_contact[3,6] <- Contact[2,5]

Population_contact[4,3] <- Contact[3,2]
Population_contact[4,5] <- Contact[3,4]
Population_contact[4,6] <- Contact[3,5]

Population_contact[5,3] <- Contact[4,2]
Population_contact[5,4] <- Contact[4,3]
Population_contact[5,6] <- Contact[4,5]

Population_contact[6,3] <- Contact[5,2]
Population_contact[6,4] <- Contact[5,3]
Population_contact[6,5] <- Contact[5,4]

# Contact between 5-14 years and children
Population_contact[3,1] <- Contact[2,1]*Pop_str[1,1]
Population_contact[3,2] <- Contact[2,1]*Pop_str[1,2]

# Contact between 15-44 years and children
Population_contact[4,1] <- Contact[3,1]*Pop_str[1,1]
Population_contact[4,2] <- Contact[3,1]*Pop_str[1,2]

# Contact between 45-64 years and children
Population_contact[5,1] <- Contact[4,1]*Pop_str[1,1]
Population_contact[5,2] <- Contact[4,1]*Pop_str[1,2]

# Contact between 64+ years and children
Population_contact[6,1] <- Contact[5,1]*Pop_str[1,1]
Population_contact[6,2] <- Contact[5,1]*Pop_str[1,2]

# Contact between children and 5-14 years
Population_contact[1,3] <- Contact[1,2]*Pop_str[1,1]
Population_contact[2,3] <- Contact[1,2]*Pop_str[1,2]

# Contact between children and 15-44 years
Population_contact[1,4] <- Contact[1,3]*Pop_str[1,1]
Population_contact[2,4] <- Contact[1,3]*Pop_str[1,2]

# Contact between children and 45-64 years
Population_contact[1,5] <- Contact[1,4]*Pop_str[1,1]
Population_contact[2,5] <- Contact[1,4]*Pop_str[1,2]

# Contact between children and 64+
Population_contact[1,6] <- Contact[1,5]*Pop_str[1,1]
Population_contact[2,6] <- Contact[1,5]*Pop_str[1,2]


# Contact between the children
Population_contact[1,2] <- sqrt(Population_contact[1,1]*Population_contact[2,2])
Population_contact[2,1] <- Population_contact[1,2]

cmDKformatted <- Population_contact
  
print(cmDKformatted)


#Save data
saveRDS(cmDKformatted, file = "Data/contactmatrix.rds")
