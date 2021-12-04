library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(plm)
library(janitor)
library(stargazer)

data <- read_csv("data/FinalDF.csv")
df <- data %>%
  janitor::clean_names()

attach(df)

Y <- cbind(life_expectancy)
X <- cbind(status,adult_mortality,infant_deaths,alcohol,percentage_expenditure,
           hepatitis_b,measles,bmi,under_five_deaths,polio,total_expenditure,
           thinness_5_9_years,income_composition_of_resources,schooling,
           emission,military_expenditure)

#Delete obviously correlated variables
X2 <- cbind(status,alcohol,percentage_expenditure,
            hepatitis_b,measles,bmi,polio,total_expenditure,
            diphtheria,hiv_aids,gdp,population,thinness_1_19_years,
            thinness_5_9_years,income_composition_of_resources,schooling,
            emission,military_expenditure)

pdf <- pdata.frame(df, index = c("country", "year"))

#Fit ols model and inspect residuals
ols <- plm(Y ~ X, data = pdf, model = "pooling")
sum_ols <- stargazer(ols, type="text")
#Omit correlated variables
ols2 <- plm(Y ~ X2, data = pdf, model = "pooling")
sum_ols2 <- stargazer(ols2, type="text")
#R2 goes up quite a bit

#Fit between model
between <- plm(Y ~ X, data = pdf, model = "between")
sum_between <- stargazer(between, type="text")
#Omit correlated variables
between2 <- plm(Y ~ X2, data = pdf, model = "between")
sum_between2 <- stargazer(between2, type="text")
#R2 goes down

#fit fixed effect model - Subtracts mean of group for each group to isolate variations within group
fixed <- plm(Y ~ X, data = pdf, model = "within")
sum_fixed <- stargazer(fixed, type="text")
#Omit correlated variables
fixed2 <- plm(Y ~ X2, data = pdf, model = "within")
sum_fixed2 <- stargazer(fixed2, type="text")
#R2 goes up

#fit random effect model
random <- plm(Y ~ X, data = pdf, model = "random")
sum_random <- stargazer(random, type="text")
#Omit correlated variables
random2 <- plm(Y ~ X2, data = pdf, model = "random")
sum_random2 <- stargazer(random2, type="text")
#R2 goes up

#extract fixed effects from fixed model
fixef(fixed)

#Breush Pagan Test - null hypothesis that the variance of the residuals is constant
lmtest::bptest(fixed)
#p-value less than 0.05 so we can reject null and conclude heteroscedacity is present
#OLS does not work with heteroscedacity #https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/

#Test which model is better
pFtest(fixed, ols)
phtest(fixed, random)
phtest(fixed, between)
#Fixed is better than random and between as null rejected - However, r2 higher with RE and between

#Check to see if Panel Effects exist in data
plmtest(ols, type=c("bp"))
#p-value is than 0.05 - we can reject null and conclude that panel effects exist in data
#Therefore should not use OLS as it disregards panel effects

#Test for cross-sectional dependence
pcdtest(fixed, test="lm")
pcdtest(fixed, test="cd")
#p-value is less than 0.05 - we reject the null and conclude that there is cross-sectional dependence.

#Breusch-Godfrey/Wooldridge test for serial correlation in panel models
pbgtest(fixed)
#Null rejected - serial correlation in idiosyncratic errors

#Durbin-Watson test for serial correlation in panel models
pdwtest(fixed)
#null rejected - residuals are autocorrelated
