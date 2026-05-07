# Final Coding
cproj1 = read.csv("fullpheno2026_Jan5copy.csv")
head(cproj1)
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


##### Modeling BEGINS 

#Hypothesis
# VPD vs Phenology + SpeciesRichness*Species
ivpd <- lme(FirstInitialGrowth ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ivpd)

lvpd <- lme(FirstLeaves ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(lvpd)

fvpd<- lme(FirstFlower ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(fvpd)

frvpd <- lme( FirstFruit ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frvpd)

rvpd <- lme( FirstRipeFruit ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rvpd)

# Temp vs Phenology + SpeciesRichness*Species
itemp <- lme(FirstInitialGrowth ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(itemp)

ltemp <- lme(FirstLeaves ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ltemp)

ftemp<- lme(FirstFlower ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(ftemp)

frtemp <- lme( FirstFruit ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frtemp)

rtemp <- lme( FirstRipeFruit ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rtemp)

#Humidity vs vs Phenology + SpeciesRichness*Species
ihum <- lme(FirstInitialGrowth ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(ihum)

lhum <- lme(FirstLeaves ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit)
anova(lhum)

fhum <- lme(FirstFlower ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit)
anova(fhum)

frhum <- lme( FirstFruit ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit)
anova(frhum)

rhum <- lme( FirstRipeFruit ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
anova(rhum)

# Best Model AICc Selection
fitListgrowth <- list(
  ivpd = lme( FirstInitialGrowth ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit),
  itemp = lme(FirstInitialGrowth ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit),
  ihum = lme(FirstInitialGrowth ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit))

fitListleaves <- list(
  lvpd = lme(FirstLeaves ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit),
  ltemp = lme(FirstLeaves ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit),
  lhum = lme(FirstLeaves ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nosol, na.action = na.omit))

fitListflower <- list(
  fvpd = lme(FirstFlower ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit),
  ftemp = lme(FirstFlower ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit),
  fhum = lme(FirstFlower ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobropoasollup, na.action = na.omit))

fitListfruit <-list(
  frvpd = lme( FirstFruit ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit),
  frtemp = lme( FirstFruit ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit),
  frhum = lme( FirstFruit ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosolsch, na.action = na.omit))

fitListripe <- list(
  rvpd = lme( FirstRipeFruit ~ AvgVPD + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit),
  rtemp = lme( FirstRipeFruit ~ AvgTempC + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit),
  rhum = lme( FirstRipeFruit ~ AvgRHperc + SpeciesRichness*Species, random = ~ 1 | Ring/Plot_Num, data = cproj1_nobrosollessch, na.action = na.omit)
)

library(AICcmodavg)

aictab(fitListgrowth)
aictab(fitListleaves)
aictab(fitListflower)
aictab(fitListfruit)
aictab(fitListripe)

########### Figures

## pheno vs microclimate excluding bad ones inigr, leav, flow, oflow, frui, rfrui 

inigr <- cproj1 %>% filter(!grepl("solri|andge|luppe|poapr", Species)) # initial growth use for which removes all that don't have enough data and all grew in the first weekinitial growth
leav<- cproj1 %>% filter(!grepl("solri|andge|luppe|poapr", Species)) # leaves
flow <- cproj1 %>% filter(!grepl("agrre|luppe|poapr|broin|solri", Species)) # flower
oflow <- cproj1 %>% filter(!grepl("broin|poapr|solri|luppe|lesca", Species)) # open flower
frui <- cproj1 %>% filter(!grepl("broin|solri|schsc|luppe", Species)) # fruit
rfrui <- cproj1 %>% filter(!grepl("broin|lesca|solri|schsc", Species)) # ripe fruit

#initial growth
gxvpd <- ggplot( inigr, aes ( x = AvgVPD, y = FirstInitialGrowth, group = Species)) +
  geom_point (aes(color = Species), size = 3) +
  geom_smooth (aes(color = Species),method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Initial Growth") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

gxvpd

gxtemp <- ggplot( inigr, aes ( x = AvgTempC, y = FirstInitialGrowth, group = Species)) +
  geom_point (aes(color = Species), size = 3) +
  geom_smooth (aes(color = Species),method = "lm") +
  labs(
    x = "Avg Plot Temp C",
    y = "Week of First Initial Growth") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

gxtemp

gxhum <- ggplot( inigr, aes ( x = AvgRHperc, y = FirstInitialGrowth, color=Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Hum % ",
    y = "Week of First Initial Growth") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

gxhum

#leaves
lxvpd <- ggplot( leav, aes ( x = AvgVPD, y = FirstLeaves, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Leaves") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

lxvpd

lxtemp <- ggplot( leav, aes ( x = AvgTempC, y = FirstLeaves, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Temp C",
    y = "Week of First Leave") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

lxtemp

lxhum <- ggplot( leav, aes ( x = AvgRHperc, y = FirstLeaves, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Hum % ",
    y = "Week of First Leaves") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

lxhum

## flower
flxvpd <- ggplot( flow, aes ( x = AvgVPD, y = FirstFlower, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

flxvpd

flxtemp <- ggplot( flow, aes ( x = AvgTempC, y = FirstFlower, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Temp C",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

flxtemp

flxhum <- ggplot( flow, aes ( x = AvgRHperc, y = FirstFlower, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Hum % ",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

flxhum

####### Fruit
frxvpd <- ggplot( frui, aes ( x = AvgVPD, y = FirstFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

frxvpd

frxtemp <- ggplot( frui, aes ( x = AvgTempC, y = FirstFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Temp C",
    y = "Week of First Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

frxtemp

frxhum <- ggplot( frui, aes ( x = AvgRHperc, y = FirstFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Hum % ",
    y = "Week of First Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

frxhum

####### Ripe Fruit
rifxvpd <- ggplot( rfrui, aes ( x = AvgVPD, y = FirstRipeFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Ripe Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

rifxvpd

rifxtemp <- ggplot( rfrui, aes ( x = AvgTempC, y = FirstRipeFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Temp C",
    y = "Week of First Ripe Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

rifxtemp

rifxhum <- ggplot( rfrui, aes ( x = AvgRHperc, y = FirstRipeFruit, color = Species)) +
  geom_point (size = 3) +
  geom_smooth (method = "lm") +
  labs(
    x = "Avg Plot Hum % ",
    y = "Week of First Ripe Fruit") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

rifxhum

# make on figure per microclimate measurment

library(patchwork)
vpd <- gxvpd + lxvpd + flxvpd + frxvpd + rifxvpd  + plot_layout(nrow = 2, ncol = 3)
vpd

temp <- gxtemp + lxtemp + flxtemp + frxtemp + rifxtemp  + plot_layout(nrow = 2, ncol = 3)
temp

hum <- gxhum + lxhum + flxhum + frxhum + rifxhum  + plot_layout(nrow = 2, ncol = 3)
hum
