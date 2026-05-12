# Week 12 assignment <- mixed models


install.packages("ggplot2")
library(ggplot2)
install.packages("lme4")
library(lme4)
library(effects)
cproj1 = read.csv("pheno_classdata_Jan29.csv")
head(cproj1)

#KL - what hypothesis are you testing? 

growthxvpd <- lmer(FirstInitialGrowth~AvgVPD + (1|Plot_Num),data=cproj1)
summary(growthxvpd)
# df = Numbers of obs (180) - (all the levels of fixed effects) - (all the levels of random effects)
df = 95-31-1
p.value = 2*pt(2.507, df = df, lower=FALSE)
p.value
# p-value = 0.01476774 <- VPD is significant 


# no clear effect of VPD on week of leaf emergence
leavexvpd <- lmer(FirstLeaves~AvgVPD + (1|Plot_Num),data=cproj1) 
summary(leavexvpd)

df = 92-31-1
p.value = 2*pt(1.786, df = df, lower=FALSE)
p.value
## p-value = 0.07915253 <- VPD is NOT significant 

gxvpd <- ggplot( cproj1, aes ( x = AvgVPD, y = FirstInitialGrowth, group = Species)) +
  geom_point (aes(color = Species), size = 3) +
  geom_smooth (aes(color = Species),method = "lm") +
  labs(
    x = "Avg Plot AvgVPD",
    y = "Week of First Initial Growth") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

gxvpd

#KL - you haven't plotted the data and model predictions from the mixed model. You can use the effects package to do this.
#you have just used a stat smooth
#need to use predict to get model predictions and then plot those with ggplot









