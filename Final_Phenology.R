# Final Coding

cproj1 = read.csv("pheno_classdata_Jan29.csv")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#using this package because the paper experiment is modeled after uses this for phenoology vs temp
install.packages("nlme")
library(nlme)
library(dplyr)

### In all analyses you want all of your variables to be either factors or numbers

#Phenology
ccproj$FirstInitialGrowth <- as.numeric(ccproj$FirstInitialGrowth)
ccproj$FirstLeaves <- as.numeric(ccproj$FirstLeaves)
ccproj$FirstFlower <- as.numeric(as.character(ccproj$FirstFlower))
ccproj$FirstOpenFlower <- as.numeric(ccproj$FirstOpenFlower)
ccproj$FirstFruit <- as.numeric(ccproj$FirstFruit)
ccproj$FirstRipeFruit <- as.numeric(ccproj$FirstRipeFruit)
ccproj$SpeciesRichness <- as.numeric(ccproj$SpeciesRichness)

#Microclimate
ccproj$AvgVPD <- as.numeric(ccproj$AvgTempC)
ccproj$AvgVPD <- as.numeric(ccproj$AvgVPD)
ccproj$AvgRHperc <- as.numeric(ccproj$AvgRHperc)


# summary subsets with data that includes species who all emegred in week 1
ccproj_nobro=subset(ccproj,ccproj$Species!="broin")
ccproj_nosol=subset(ccproj,ccproj$Species!="solri") # use for initial growth and leaves
ccproj_nobropoasollup <- ccproj %>% filter(!grepl("broin|poapr|solri|luppe", Species)) # use for flower
ccproj_wandaggsch <- ccproj %>% filter(!grepl("broin|poapr|solri|luppe|lesca", Species)) # use for open flower
ccproj_nobrosolsch <- ccproj %>% filter(!grepl("broin|solri|schsc", Species)) # use for fruit
ccproj_nobrosollessch <- ccproj %>% filter(!grepl("broin|lesca|solri|schsc", Species)) # use for ripe fruit


# VPD vs Phenology
itemp <- lme(FirstInitialGrowth ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = ccproj_nosol, na.action = na.omit)
anova(itemp)

ltemp <- lme(FirstLeaves ~ AvgVPD + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = ccproj_nosol, na.action = na.omit)
anova(ltemp)

ftemp<- lme(FirstFlower ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = ccproj_nobropoasollup, na.action = na.omit)
anova(ftemp)

frtemp <- lme( FirstFruit ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = ccproj_nobrosolsch, na.action = na.omit)
anova(ftemp)

rtemp <- lme( FirstRipeFruit ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = ccproj_nobrosollessch, na.action = na.omit)
anova(rtemp)

