library(readr)

life <- read_csv("data/life_expectancy.csv")
emission <- read_csv("data/emission_data.csv")[0,251:266]
emi
env_var <- read_csv("data/environmental_variables.csv")
names(emission) <-  