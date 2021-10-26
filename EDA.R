library(readr)
library(dplyr)
library(caret)
library(Amelia)
library(moments)
library(psych)
library(corrplot)

#Load datasets
life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015

#Save descriptive statistics as dataframe using psych package
life_summary <- as.data.frame(describe(life))

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
life_imputed[6:23] <- scale(life_imputed[6:23], center = TRUE, scale = TRUE)

#Correlation table
life_cor <- cor(life_imputed[6:22])
life_cor_matrix <- as.data.frame(life_cor)
palette <- colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = life_cor, col = palette, symm = TRUE)

#Outlier detection - Fit simple model to calculate Cook's distance
simple_model <- lm(life_imputed$`Life expectancy` ~ .,, data = life_imputed)
cooks <- cooks.distance(simple_model)
#filter out values with over 4 times the mean cook's distance
outliers <- cooks[cooks > mean(cooks) * 4]

