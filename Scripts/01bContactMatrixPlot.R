rm(list = ls())

#Load packages
library(tidyverse)

#Load augmented contact matrix
cmDK <- readRDS(file = "Data/contactmatrix.rds")
cmDK <- as.data.frame(cmDK)


#Create data frame for plotting
cmDKTibble <- tibble(ageGroupContact = rep(c("<1", "1-5", "6-14", "15-44", "45-64", "65+"), each = 6),
                     ageGroup = rep(c("<1", "1-5", "6-14", "15-44", "45-64", "65+"), 6),
                     nContacts = as_vector(flatten(cmDK)))

#Turn age groups into factors
cmDKTibble$ageGroup <- factor(cmDKTibble$ageGroup, ordered=TRUE, 
                              levels = c("<1","1-5", "6-14", "15-44", "45-64", "65+"))

cmDKTibble$ageGroupContact <- factor(cmDKTibble$ageGroupContact, ordered=TRUE, 
                                     levels = c("<1","1-5", "6-14", "15-44", "45-64", "65+"))

#Plot of the contact matrix
contactMat <- ggplot(cmDKTibble, aes(x = ageGroupContact, y = ageGroup)) +
  geom_tile(aes(fill = as_vector(flatten(cmDK)))) +
  geom_text(aes(label = round(as_vector(flatten(cmDK)), 3))) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "Contact matrix for the Danish population",
       x = "Age group of contact",y = "Age group of individual",
       fill = "Number of contacts\nper day per individual")+
  theme_bw(base_size = 12)

print(contactMat)
#Save
ggsave(contactMat,filename = "Plots/contactMat.png", height = 3, width = 7)


