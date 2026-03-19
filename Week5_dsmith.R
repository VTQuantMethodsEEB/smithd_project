## Week 5 Assignment: Permutation tests

# Hypothesis 1: High-richness plots have greater mean SpeciesRichness than low-richness plots.
# Hypothesis 2: High species richness will decrease plot temperature more than low species richness

cproj1 = read.csv("pheno_classdata_Jan29.csv")

library(ggplot2)
theme_set(theme_bw())

set.seed(101) # set seed so that r random number generator will start at the same place
res <- NA # place holder for results

#view data
hist(cproj1$SpeciesRichness)

#create grouping variable
cproj1$rich.cat = "high"
cproj1$rich.cat[cproj1$SpeciesRichness<12] = "low"

#permutation loop
for (i in 1:10000) {
  phenoboot <- sample(cproj1$rich.cat) ## scramble (randomly shuffle everything to break up relationship between group and data)
  
  n_high <- length(cproj1$rich.cat[cproj1$rich.cat=="high"])
  
  highboot <- cproj1$SpeciesRichness[phenoboot == "high"]
  lowboot  <- cproj1$SpeciesRichness[phenoboot == "low"]
  
  res[i] <- mean(highboot, na.rm = TRUE) - mean(lowboot, na.rm = TRUE) # compute difference in means
}
  

#observed data in actual data with no shuffling
obs <- mean(cproj1$SpeciesRichness[cproj1$rich.cat == "high"], na.rm = TRUE) -
    mean(cproj1$SpeciesRichness[cproj1$rich.cat == "low"], na.rm = TRUE)
obs

# T Test

t.test(AvgTempC ~ rich.cat, data = cproj1, alternative = "less")
