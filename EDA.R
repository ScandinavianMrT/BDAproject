library(readr)
library(dplyr)
library(caret)

#Load datasets
life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- select(read_csv("data/emission_data.csv"), 1, 251:266) # delete all columns except country and 2000:2015
military <- select(read_csv("data/military_expenditure.csv"), 1, 45:60) # delete all columns except country and 2000:2015

#Change categorical value of Status variable to binary
life$Status <- ifelse(life$Status == "Developed", 1,0)

#As life expecancy is our dependend variable and only 10 observations are missing, we will simply drop those
life <- life[complete.cases(life_stand$`Life expectancy`), ]

new_life <- life %>%  group_by(Country) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

#Standardize data to have unit variance and zero mean as data variables are 
# given in incomensurable units and regression model will rely on distance measure
life_preproc <- preProcess(select(life, 1, 4:22), method=c("center", "scale"))
life_stand <- predict(life_preproc, select(life, 1, 3:22))



#for(i in 1:ncol(life)){
# life %>% group_by("Country") 
#life[is.na(life[,i]), i] <- mean(data[,i], na.rm = TRUE)
#}

#Alcohol has many NaN values, so we fill NaN with average value from observations from same country
#for (colname in colnames(life)) {
 # test <- life %>% 
  #  group_by(Country) %>% 
   # mutate(life$colname) <- ifelse(is.na(Alcohol), mean(Alcohol,na.rm=TRUE), Alcohol))
#}

