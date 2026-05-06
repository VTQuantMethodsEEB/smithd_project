# Final Coding

cproj1 = read.csv("pheno_classdata_Jan29.csv")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#
install.packages("nlme")
library(nlme)
library(dplyr)

### In all analyses you want all of your variables to be either factors or numbers

#Phenology
cproj1$FirstInitialGrowth <- as.numeric(cproj1$FirstInitialGrowth)
cproj1$FirstLeaves <- as.numeric(cproj1$FirstLeaves)
cproj1$FirstFlower <- as.numeric(as.character(cproj1$FirstFlower))
cproj1$FirstOpenFlower <- as.numeric(cproj1$FirstOpenFlower)
cproj1$FirstFruit <- as.numeric(cproj1$FirstFruit)
cproj1$FirstRipeFruit <- as.numeric(cproj1$FirstRipeFruit)
cproj1$SpeciesRichness <- as.numeric(cproj1$SpeciesRichness)

#Microclimate
cproj1$AvgVPD <- as.numeric(cproj1$AvgTempC)
cproj1$AvgVPD <- as.numeric(cproj1$AvgVPD)
cproj1$AvgRHperc <- as.numeric(cproj1$AvgRHperc)


# summary subsets with data that excludes species who all emerged in week 1
cproj1_nobro=subset(cproj1,cproj1$Species!="broin")
cproj1_nosol=subset(cproj1,cproj1$Species!="solri") # use for initial growth and leaves
cproj1_nobropoasollup <- cproj1 %>% filter(!grepl("broin|poapr|solri|luppe", Species)) # use for flower
cproj1_wandaggsch <- cproj1 %>% filter(!grepl("broin|poapr|solri|luppe|lesca", Species)) # use for open flower
cproj1_nobrosolsch <- cproj1 %>% filter(!grepl("broin|solri|schsc", Species)) # use for fruit
cproj1_nobrosollessch <- cproj1 %>% filter(!grepl("broin|lesca|solri|schsc", Species)) # use for ripe fruit


##### CODING BEGINS 

Hypothesis
# VPD vs Phenology + SpeciesRichness*Species
ivpd <- lme(FirstInitialGrowth ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ivpd)

lvpd <- lme(FirstLeaves ~ AvgVPD + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(lvpd)

fvpd<- lme(FirstFlower ~ AvgVPD + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(fvpd)

frvpd <- lme( FirstFruit ~ AvgVPD + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frvpd)

rvpd <- lme( FirstRipeFruit ~ AvgVPD + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rvpd)

# Temp vs Phenology + SpeciesRichness*Species
itemp <- lme(FirstInitialGrowth ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(itemp)

ltemp <- lme(FirstLeaves ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ltemp)

ftemp<- lme(FirstFlower ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(ftemp)

frtemp <- lme( FirstFruit ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frtemp)

rtemp <- lme( FirstRipeFruit ~ AvgTempC + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rtemp)

#Humidity vs vs Phenology + SpeciesRichness*Species
ihum <- lme(FirstInitialGrowth ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ihum)

lhum <- lme(FirstLeaves ~ AvgRHperc + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(lhum)

fhum <- lme(FirstFlower ~ AvgRHperc + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(fhum)

frhum <- lme( FirstFruit ~ AvgRHperc + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frhum)

rhum <- lme( FirstRipeFruit ~ AvgRHperc + Species*SpeciesRichness, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rhum)
