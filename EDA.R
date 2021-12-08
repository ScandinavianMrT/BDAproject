library(readr)
library(plyr)
library(dplyr)
library(ggpubr)
library(tidyr)
library(caret)
library(Amelia)
library(moments)
library(psych)
library(corrplot)
library(imputeTS)
library(data.table)

#Load datasets
life <- read_csv("data/Life_Expectancy_Data_2.0.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015
HappinessMean <- select(read_csv("data/happiness.csv"),1,8)

#Convert emission and military data to long format for merging, renaming some variables
emission_long <- melt(setDT(emission), id.vars = "Country")[order(Country, -variable),]
names(emission_long)[names(emission_long) == 'variable'] <- 'Year'
names(emission_long)[names(emission_long) == 'value'] <- 'Emission'
military_long <- melt(setDT(military), id.vars = "Name")[order(Name, -variable),]
names(military_long)[names(military_long) == 'Name'] <- 'Country'
names(military_long)[names(military_long) == 'variable'] <- 'Year'
names(military_long)[names(military_long) == 'value'] <- 'Military Expenditure'
names(HappinessMean)[names(HappinessMean) == 'Mean'] <- 'Mean happiness'


#Merge military, emission and happiness dataset with life dataset
life_emiss <- merge(life, emission_long, by = c("Country", "Year"))
df1 <- merge(life_emiss, military_long, by = c("Country", "Year"))


#Save descriptive statistics as dataframe using psych package - #HOW TO GET CORRECT VALUES???
df_summary <- as.data.frame(describe(df1), digits=2)

#Impute NA's using mean of respective countries in places where some values are present for that country
missmap(df)
df_imp1 <- df1 %>%  group_by(Country) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#Impute remaining NA values by mean of column using imputeTS package
df_imp2 <- na_mean(df_imp1)
missmap(df_imp2)

#Change categorical value of Status variable to binary
df_imp2$Status <- ifelse(df_imp2$Status == "Developed", 1,0)

#Outlier detection
boxplot(df_imp2[,4]) #Haiti 36
boxplot(df_imp2[,6]) #Infant death India
boxplot(df_imp2[,10])#Measles weird values
boxplot(df_imp2[,25])#Example for no outliers

#Change Wrong Value for Haiti 2010
which(df_imp2 == 36.3, arr.ind=TRUE)
df_imp2[994,4]=60.5
print(df_imp2[994,4])

#Change wrong Values for India Infant death
which(df_imp2== "India", arr.ind=TRUE)
df_imp2[1058,6]=66.7
df_imp2[1059,6]=64.4
df_imp2[1060,6]=62.2
df_imp2[1061,6]=60
df_imp2[1062,6]=57.8
df_imp2[1063,6]=55.7
df_imp2[1064,6]=53.7
df_imp2[1065,6]= 51.6
df_imp2[1066,6]=49.4
df_imp2[1067,6]=47.3
df_imp2[1068,6]=45.1
df_imp2[1069,6]=43
df_imp2[1070,6]=40.9
df_imp2[1071,6]=38.8
df_imp2[1072,6]=36.9
df_imp2[1073,6]=34.9

#Check normality
ggdensity(df_imp2$`Life expectancy`, main="Life Expectancy Distribution", 
          xlab="Life expectancy")
ggqqplot(df_imp2$Alcohol,main="Distribution of Alcohol Consumption", 
         xlab="Consumption")  +scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

#Standardize data to have unit variance and zero mean as data variables are 
#given in incomensurable units
df_stand <- df_imp2
df_stand[5:24] <- scale(df_stand[5:24], center = TRUE, scale = TRUE)

#Correlation table and plot
df_cor <- cor(df_imp2[4:24])
cor_matrix <- as.data.frame(df_cor)
corrplot(df_cor, method = 'color')

#Export datatable
write.csv(df_stand,"data/FinalDf.csv", row.names = FALSE)

#Create new dataset with years aggregated and happiness added
df_new <- aggregate(df_stand[, 3:22], list(df_stand$Country), mean)
names(df_new)[names(df_new) == 'Group.1'] <- 'Country'
df_new <- merge(df_new,HappinessMean, by=c("Country"), all.x = TRUE)

#Impute missing values of happiness data by mean of column
df_new$`Mean happiness` <- na_mean(df_new$`Mean happiness`)

#Export datatable
write.csv(df_new,"data/aggregated_data.csv", row.names = FALSE)

