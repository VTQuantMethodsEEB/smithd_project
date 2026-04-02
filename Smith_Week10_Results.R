#Week 10 - Part 1 GLMS  Smith_Week10_Results

cproj1 = read.csv("pheno_classdata_Jan29.csv")

#HYPOTHESIS 1: Species Richness + Species predicts development stage

#created a new dataframe to see just these three column so that I could tell what kind of 
new_df <- cproj1[, c("SpeciesRichness", "Species", "FirstInitialGrowth")]
new_df



grichxgr = glm(FirstInitialGrowth ~ SpeciesRichness + Species, 
               family = "Gamma", 
               data = cproj1)
summary(grichxgr)
# species riches and certain species (andge,poapr, and maybe luppe) can predict the rate of first initial growth



# VPD and species seems to explain rate of leaves
gVPDxgr = glm(FirstInitialGrowth ~ AvgVPD + Species, 
               family = "Gamma", 
               data = cproj1)
summary(gVPDxgr)

gVPDxle = glm(FirstLeaves ~ AvgVPD + Species, 
              family = "Gamma", 
              data = cproj1)
summary(gVPDxle)
























































































































