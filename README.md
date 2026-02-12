# smithd_project
#### hey girl tesing for pushing


# data narrative
##I collected phenological data (initial growth, flowering, etc.) on herbaceous grasses over 10 weeks. 
##I had 8 target species and collected across different levels of species richness: monoculture, 9-species polyculture, and 16-species polyculture. 
##I also collected microclimate data organized on the datasheet as Avg, Max, and Min. 
##I then created equations to calculate the first time a species reached a particular phenological stage. 
##The goal for this dataset is to use models to understand how species and species richness affect the different microclimate measurements, respectively. 
##I also want to use this dataset to understand how species richness and species affect the different phenological stages. 
##I want to create models to measure how species richness, species, and the respective microclimate measurements affect the phenological stage. 
##Lastly, I would like to create figures based on the most significant model using ggplot.

cproj1 = read.csv("pheno_classdata_Jan29.csv")
head(cproj1)

install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)

dinitial1 = cproj1 %>% 
  group_by(Species) %>% 
  summarise(mean=mean(FirstInitialGrowth,na.rm=TRUE))
  
##ogranized the data by species and species richness for phenological stage intial growth
##I then used the summarise function to get the mean, standard deviation, and standard error

dinitial2= cproj1 %>%
  group_by(Species,as.factor(SpeciesRichness)) %>%
  summarise(
    N=sum(!is.na(FirstInitialGrowth)),
    mean=mean(FirstInitialGrowth,na.rm=TRUE),
    sd=sd(FirstInitialGrowth,na.rm=TRUE),
    se=sd/sqrt(N),
    .groups = "drop" # drop groups according to gpt even tho isn't required to avoid hidden grouping which causes weird ggplot/unexpected summaries
  )
  
 ## mutate will organize by modifying or creating a column
dinitial <- cproj1 %>%
  mutate(growth_change = Week10InitialGrowth - Week1InitialGrowth)
dinitial













