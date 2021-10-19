library(readr)
library(dplyr)

life <- read_csv("data/life_expectancy.csv")
env_var <- read_csv("data/environmental_variables.csv")
emission <- read_csv("data/emission_data.csv")
emission_new <- select(emission, -2:-250) # delete all columns except country and 2000:2017


