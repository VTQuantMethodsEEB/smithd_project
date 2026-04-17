## Linear Models Week 7 and 8

cproj1 = read.csv("pheno_classdata_Jan29.csv")

# Hypothesis: Does species richness predict VPD?
lm_pheno <- lm(AvgVPD ~ SpeciesRichness, data=cproj1, na.action = na.exclude)
anova(lm_pheno)
summary(lm_pheno)

# For every +1 increase in species richness, AvgVPD decreases by ~0.020
#significant strong- no chance this is random

hist(cproj1$SpeciesRichness)
hist(cproj1$AvgVPD)
# not normal


lm_pheno$residuals
shapiro.test(resid(lm_pheno))
plot(lm_pheno)
#there may be some non-linearity based on residuals vs fitted model
# Q-Q: the residuals do seems to be normal
# scale-location: there appears to be some heteroscadicty but not much
# residuals vs leverage: no outliers

library(ggplot2)

rvpdrich <- ggplot(cproj1, aes(x = SpeciesRichness, y = AvgVPD)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  labs(
    x = "Species Richness",
    y = "Average VPD"
    ) +
  theme_bw()

rvpdrich
# as species richness increases, VPD decreases, confidence interval is small meaning the relationship is probably true


#  Does species richness and species predict VPD?
# using linear model to test if species richness and species have an affect on AVGVPD


lm_pheno <- lm(AvgVPD ~ SpeciesRichness, data=cproj1, na.action = na.exclude)
summary(lm_pheno)

lm_pheno2 <- lm(AvgVPD ~ SpeciesRichness + Species, data=cproj1, na.action = na.exclude)
summary(lm_pheno2)
# species richness does affect VPD but species doesn't

plot(lm_pheno2)
# adding spcecies did not fix nonlinearity
# Q-Q: the residuals still normal
# scale-location: some heteroscadicty but not much
# residuals vs leverage: no outliers

# species richness does affect VPD but species doesn't
lm_pheno3 <- lm(AvgVPD ~ SpeciesRichness*Species, data=cproj1, na.action = na.exclude)
summary(lm_pheno3)

plot(lm_pheno3)
# interaction did not fix nonlinearity
# Q-Q: the residuals still normal
# scale-location: some heteroscadicty but not much
# species introduced some points that influencing slope 

#only getting blank
library(performance)
p <- check_model(lm_pheno)
p
#kl - this worked for me?

































































