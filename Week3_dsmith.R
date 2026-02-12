# Week 3 Assignment

## For these figures I want to visulaize how microclimate measurments influence phenology

install.packages("ggplot2")
library(ggplot2)

#set variables all as numeric
cproj1$FirstInitialGrowth <- as.numeric(cproj1$FirstInitialGrowth)
cproj1$FirstLeaves <- as.numeric(cproj1$FirstLeaves)
cproj1$FirstFlower <- as.numeric(as.character(cproj1$FirstFlower))
cproj1$FirstOpenFlower <- as.numeric(cproj1$FirstOpenFlower)
cproj1$FirstFruit <- as.numeric(cproj1$FirstFruit)
cproj1$FirstRipeFruit <- as.numeric(cproj1$FirstRipeFruit)
cproj1$SpeciesRichness <- as.numeric(cproj1$SpeciesRichness)

#Microclimate
cproj1$AvgTempC <- as.numeric(cproj1$AvgTempC)
cproj1$AvgVPD <- as.numeric(cproj1$AvgVPD)
cproj1$AvgRHperc <- as.numeric(cproj1$AvgRHperc)


##PLOTS
rhxflower <- ggplot( cproj1, aes ( x = AvgRHperc, y = FirstFlower)) +
  geom_smooth ( color = "hotpink", method = "lm") +
  geom_point()+
  labs(
    x = "Avg Plot Relative Humidity",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
rhxflower


tempxflower <- ggplot( cproj1, aes ( x = AvgTempC, y = FirstFlower)) +
  geom_point()+
  geom_smooth ( color = "hotpink", method = "lm") +
  labs(
    x = "Avg Plot Temp",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
tempxflower


vpdxflower <- ggplot( cproj1, aes ( x = AvgVPD, y = FirstFlower)) +
  geom_point()+
  geom_smooth ( color = "hotpink", method = "lm") +
  labs(
    x = "Avg Plot VPD",
    y = "Week of First Flower") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
vpdxflower
