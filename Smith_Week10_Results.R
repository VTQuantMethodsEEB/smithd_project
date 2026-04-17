#Week 10 - Part 1 GLMS  Smith_Week10_Results

cproj1 = read.csv("pheno_classdata_Jan29.csv")

#HYPOTHESIS 1:  VPD + Species Richness can predict rate of emergence of leaves

#created a new dataframe to see just these three column so that I could tell what kind of data I have
new_df <- cproj1[, c("SpeciesRichness", "Species", "FirstLeaves", "AvgVPD")]
new_df


vpdxl = glm(FirstLeaves ~ AvgVPD, 
               family = "Gamma", 
               data = cproj1)
summary(vpdxl)
# AvgVPD was not a significant predictor of FirstLeaves


# speciesrichness seems to explain rate of leaves
srVPDxl = glm(FirstLeaves ~ AvgVPD + SpeciesRichness, 
              family = "Gamma", 
               data = cproj1)
summary(srVPDxl)

# There was no evidence that the interaction between AvgVPD and SpeciesRichness significantly influenced FirstLeaves
srVPDxl2 = glm(FirstLeaves ~ AvgVPD*SpeciesRichness, 
              family = "Gamma", 
              data = cproj1)
summary(srVPDxl2)


dat.new = expand.grid(
  AvgVPD = seq(min(cproj1$AvgVPD), max(cproj1$AvgVPD), length.out = 100),
  SpeciesRichness = mean(cproj1$SpeciesRichness, na.rm = TRUE)
)
dat.new$yhat = predict(vpdxl, type = "response", newdata = dat.new)
dat.new$yhat


library(effects)
plot(allEffects(srVPDxl2))
plot(allEffects(srVPDxl))
plot(allEffects(vpdxl))



anova(vpdxl,srVPDxl)
anova(vpdxl,srVPDxl2)
anova(srVPDxl,srVPDxl2)

# Likelihood ratio tests
# Adding SpeciesRichness significantly improved model than VPD alone; species richness predicts some variation in FirstLeaves
# The interaction model did not significantly improve model; no evidence that effects of AvgVPD depends on SpeciesRichness.

fitListpheno <- list(
  vpdxl = glm(FirstLeaves ~ AvgVPD, 
                family = "Gamma", 
                data = cproj1),
  srVPDxl = glm(FirstLeaves ~ AvgVPD + SpeciesRichness, 
                family = "Gamma", 
                data = cproj1),
  
  srVPDxl2 = glm(FirstLeaves ~ AvgVPD*SpeciesRichness, 
                 family = "Gamma", 
                 data = cproj1)
  
)

library(AICcmodavg)
aictab(fitListpheno)

#best fit model includes AvgVPD and Species Richness as predictors of phenology
















































































































