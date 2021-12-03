library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(plm)
library(janitor)
library(stargazer)

#disable scientific notation
options(scipen = 100)

#load data and clean names
data <- read_csv("data/data_proc.csv")
df <- data %>%
  janitor::clean_names()

#Fit ols model and inspect residuals
ols <- plm(life_expectancy ~ status+adult_mortality+infant_deaths+alcohol+percentage_expenditure+hepatitis_b+measles+bmi+under_five_deaths+polio+total_expenditure+diphtheria+hiv_aids+gdp+population+thinness_1_19_years+thinness_5_9_years+income_composition_of_resources+schooling+emission+military_expenditure, data = df, model = "pooling", index = c("country", "year"))
sum_ols <- stargazer(ols, type="text")
ols_residuals = residuals(ols)
ols_fit = fitted(ols)
qplot(ols_fit, ols_residuals)

#fit fixed effect model
fixed <- plm(life_expectancy ~ status+adult_mortality+infant_deaths+alcohol+percentage_expenditure+hepatitis_b+measles+bmi+under_five_deaths+polio+total_expenditure+diphtheria+hiv_aids+gdp+population+thinness_1_19_years+thinness_5_9_years+income_composition_of_resources+schooling+emission+military_expenditure, data = df, model = "within", index = c("country", "year"))
sum_fixed <- stargazer(fixed, type="text")
print(sum_fixed)

#fit random effect model
random <- plm(life_expectancy ~ status+adult_mortality+infant_deaths+alcohol+percentage_expenditure+hepatitis_b+measles+bmi+under_five_deaths+polio+total_expenditure+diphtheria+hiv_aids+gdp+population+thinness_1_19_years+thinness_5_9_years+income_composition_of_resources+schooling+emission+military_expenditure, data = df, model = "random", index = c("country", "year"))
sum_rand <- stargazer(random, type="text")
print(sum_rand)

#Breush Pagan Test - null hypothesis that the variance of the residuals is constant
lmtest::bptest(ols)
#p-value less than 0.05 so we can reject null and conclude heteroscedasticity is present
#ols does not work with heteroscedacity #https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/

#Test which model is better
pFtest(fixed, ols)
phtest(fixed, random)
#Fixed is better than random as null hypo rejected

#extract fixed effets from model
fixef(fixed)

#Check to see if Panel Effects exist in data
plmtest(ols, type=c("bp"))
#p-value is than 0.05 - we can reject null hypothesis conclude that panel effects exist in data

#Test for cross-sectional dependence
pcdtest(fixed, test="lm")
pcdtest(fixed, test="cd")
#p-value is less than 0.05 - we reject the null and conclude that there is cross-sectional dependence.

#test for serial correlation
pbgtest(fixed)
