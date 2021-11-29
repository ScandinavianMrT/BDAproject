library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(Amelia)
library(moments)
library(psych)
library(corrplot)
library(imputeTS)
library(data.table)

#Load datasets
life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015
HappinessMean <- select(read_csv("data/Hapmean.csv"),2,9)

#Convert emission and military data to long format for merging, renaming some variables
emission_long <- melt(setDT(emission), id.vars = "Country")[order(Country, -variable),]
names(emission_long)[names(emission_long) == 'variable'] <- 'Year'
names(emission_long)[names(emission_long) == 'value'] <- 'Emission'
military_long <- melt(setDT(military), id.vars = "Name")[order(Name, -variable),]
names(military_long)[names(military_long) == 'Name'] <- 'Country'
names(military_long)[names(military_long) == 'variable'] <- 'Year'
names(military_long)[names(military_long) == 'value'] <- 'Military Expenditure'

#Merge military, emission and happiness dataset with life dataset
life_emiss <- merge(life, emission_long, by = c("Country", "Year"))
df1 <- merge(life_emiss, military_long, by = c("Country", "Year"))
df <- merge (df1,HappinessMean, by=c("Country"), all.x = TRUE)

#Save descriptive statistics as dataframe using psych package - #HOW TO GET CORRECT VALUES???
df_summary <- as.data.frame(describe(df), digits=2)

#Impute NA's using mean of respective countries in places where some values are present for that country
missmap(df)
df_imp1 <- df %>%  group_by(Country) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#Impute remaining NA values by mean of column using imputeTS package
df_imp2 <- na_mean(df_imp1)
missmap(df_imp2)

#Change categorical value of Status variable to binary
df_imp2$Status <- ifelse(df_imp2$Status == "Developed", 1,0)

#Standardize data to have unit variance and zero mean as data variables are 
# given in incomensurable units and regression model will rely on distance measure
df_stand <- df_imp2
df_stand[4:24] <- scale(df_stand[4:24], center = TRUE, scale = TRUE)

#Correlation table and plot
df_cor <- cor(df_imp2[4:24])
cor_matrix <- as.data.frame(df_cor)
corrplot(df_cor, method = 'color')

