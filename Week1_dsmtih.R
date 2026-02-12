#Week 1 Assignment

## read in data 
pheno <- read.csv("pheno_classdata_Jan29.csv")

## view data
view(pheno) # opens data in a new window

## view first five rows 
head(pheno) # each row refers to the five five rows of data for plot 67

## look at structure of data( are they factors, characters, etc)
str(pheno$AvgVPD) #num
str(pheno$Species) #chr
str(pheno$SpeciesRichness) #int

## dimensions, meaning how many rows do you hae for each column
dim(pheno) #124 rows x #273 columns

## used to filter a data frame to select only the rows where the named(Species) column has the exact value "agree", effectively subsetting your data to show only records for the "agree" species
pheno[pheno$Species=="agrre",]
pheno[pheno$Species=="agrre"&pheno$SpeciesRichness=="16",]
pheno[pheno$Species=="agrre" | pheno$SpeciesRichness=="16",] # way cleaner then 1st two

## summary data gives you the summary stats of each column
summary(pheno)


## add column with calculated data
pheno$addition.result = pheno$AvgTempC + 10
head(pheno) #dataframe has many columns to verify new column using this code
tail(names(pheno)) # use this to see last five columns to verify if add column added was successful
dim(pheno) #124 x 274 extra column added

## aggregate <- average (mean) timing of ripe fruit for each species at that species richness level
ph_mean <- aggregate(FirstRipeFruit ~ Species + SpeciesRichness,
                     FUN = mean,
                     data = pheno)
ph_mean

