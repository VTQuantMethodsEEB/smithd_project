#WEEK 2 ASSIGNMENT

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
