library(readr)
library(dplyr)
library(caret)
library(Amelia)
library(moments)

#Load datasets
life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015

#Impute NaN's using Amelia, leaving out first 3 columns in process
missmap(life)
life_amelia <- amelia(as.data.frame(life), m = 3, idvars = c("Country","Year","Status"))
write.amelia(obj = life_amelia, file.stem = "life_imp")
life_imputed = read_csv("life_imp3.csv")
missmap(life_imputed)

#Change categorical value of Status variable to binary
life_imputed$Status <- ifelse(life_imputed$Status == "Developed", 1,0)

#Standardize data to have unit variance and zero mean as data variables are 
# given in incomensurable units and regression model will rely on distance measure
life_imputed[5:23] <- scale(life_imputed[5:23])

