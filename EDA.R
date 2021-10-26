library(readr)
library(plyr)
library(dplyr)
library(caret)
library(Amelia)
library(moments)
library(psych)
library(corrplot)
library(imputeTS)

#Load datasets
life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015

#Save descriptive statistics as dataframe using psych package
life_summary <- as.data.frame(describe(life))

#Impute NA's using mean of respective countries in places where some values are present for that country
missmap(life)
life_imp1 <- life %>%  group_by(Country) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#Impute remaining NA values by mean of column using imputeTS package
life_imp2 <- na_mean(life_imp1)
missmap(life_imp2)

#Change categorical value of Status variable to binary
life_imp2$Status <- ifelse(life_imp2$Status == "Developed", 1,0)

#Standardize data to have unit variance and zero mean as data variables are 
# given in incomensurable units and regression model will rely on distance measure
life_stand <- life_imp2
life_stand[4:22] <- scale(life_stand[4:22], center = TRUE, scale = TRUE)

#Correlation table and plot
life_cor <- cor(life_imp2[4:22])
life_cor_matrix <- as.data.frame(life_cor)
palette <- colorRampPalette(c("green", "white", "red")) (20)
corrplot(life_cor, method = 'color')
