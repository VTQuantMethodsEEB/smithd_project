#Week 1 Assignment

## read in data 
setwd("~/Documents/PHD Classes /2025-2026/Spring 2026/BIOL 5984 Quantative Methods/smithd_project")
cproj1 = read.csv("pheno_classdata_Jan29.csv")
head(cproj1) 
## view data
view(cproj1) # opens data in a new window

## view first five rows 
head(cproj1) # each row refers to the five five rows of data for plot 67

## look at structure of data( are they factors, characters, etc)
str(cproj1$AvgVPD) #num
str(cproj1$Species) #chr
str(cproj1$SpeciesRichness) #int

## dimensions, meaning how many rows do you hae for each column
dim(cproj1) #124 rows x #273 columns

## used to filter a data frame to select only the rows where the named(Species) column has the exact value "agree", effectively subsetting your data to show only records for the "agree" species
cproj1[cproj1$Species=="agrre",]
cproj1[cproj1$Species=="agrre"&cproj1$SpeciesRichness=="16",]
cproj1[cproj1$Species=="agrre" | cproj1$SpeciesRichness=="16",] # way cleaner then 1st two

## summary data gives you the summary stats of each column
summary(cproj1)


## add column with calculated data
cproj1$addition.result = cproj1$AvgTempC + 10
head(cproj1) #dataframe has many columns to verify new column using this code
tail(names(cproj1)) # use this to see last five columns to verify if add column added was successful
dim(cproj1) #124 x 274 extra column added

## aggregate <- average (mean) timing of ripe fruit for each species at that species richness level
ph_mean <- aggregate(FirstRipeFruit ~ Species + SpeciesRichness,
                     FUN = mean,
                     data = cproj1)
ph_mean

