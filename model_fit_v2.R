library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(plm)
library(janitor)
library(stargazer)
library(ggplot2)
library(esquisse)

data <- read_csv("data/FinalDF.csv")
df <- data %>%
  janitor::clean_names()

attach(df)

Y <- cbind(life_expectancy)
X <- cbind(status,adult_mortality,infant_deaths,alcohol,percentage_expenditure,
           hepatitis_b,measles,bmi,under_five_deaths,polio,total_expenditure,
           thinness_5_9_years,income_composition_of_resources,schooling,
           emission,military_expenditure)

#Delete obviously correlated variables: adult_mortality,infant_deaths and under_five_deaths
X2 <- cbind(status,alcohol,percentage_expenditure,
            hepatitis_b,measles,bmi,polio,total_expenditure,
            diphtheria,hiv_aids,gdp,population,thinness_1_19_years,
            thinness_5_9_years,income_composition_of_resources,schooling,
            emission,military_expenditure)

pdf <- pdata.frame(df, index = c("country", "year"))
attach(pdf)

#Fit ols model and inspect residuals
ols <- plm(Y ~ X, data = pdf, model = "pooling")
sum_ols <- stargazer(ols, type="text", title = "OLS model - full data - Summary statistics", out = "Results/OLS model - full data - Summary statistics.txt")
pdf$ols_preds <- predict(ols)
pdf$ols_residuals <- as.list.data.frame(residuals(ols))
#Omit correlated variables
ols2 <- plm(Y ~ X2, data = pdf, model = "pooling")
sum_ols2 <- stargazer(ols2, type="text", title = "OLS model - omitted variables - Summary statistics", out = "Results/OLS model - omitted variables - Summary statistics.txt")
pdf$ols2_preds <- predict(ols2)
pdf$ols2_residuals <- as.list.data.frame(residuals(ols2))

#fit fixed effect model - Subtracts mean of group for each group to isolate variations within group
fixed <- plm(Y ~ X, data = pdf, model = "within")
sum_fixed <- stargazer(fixed, type="text", title = "Fixed effects model - Full data - Summary statistics", out = "Results/Fixed effects - Full data - Summary statistics.txt")
pdf$fixed_preds <- predict(fixed)
pdf$fixed_residuals <- as.list.data.frame(residuals(fixed))
#Omit correlated variables
fixed2 <- plm(Y ~ X2, data = pdf, model = "within")
sum_fixed2 <- stargazer(fixed2, type="text", title = "Fixed effects - omitted variables - Summary statistics", out = "Results/Fixed effects - omitted variables - Summary statistics.txt")
pdf$fixed2_preds <- predict(fixed2)
pdf$fixed2_residuals <- as.list.data.frame(residuals(fixed2))

#fit random effect model
random <- plm(Y ~ X, data = pdf, model = "random")
sum_random <- stargazer(random, type="text", title = "Random effects model - omitted variables - Summary statistics", out = "Results/Random effects model - Full data - Summary statistics.txt")
pdf$random_preds <- predict(random)
pdf$random_residuals <- as.list.data.frame(residuals(random))
#Omit correlated variables
random2 <- plm(Y ~ X2, data = pdf, model = "random")
sum_random2 <- stargazer(random2, type="text", title = "Random effects model - omitted variables - Summary statistics", out = "Results/Random effects model - omitted variables - Summary statistics.txt")
pdf$random2_preds <- predict(random2)
pdf$random2_residuals <- as.list.data.frame(residuals(random2))

#Breush Pagan Test - null hypothesis that the variance of the residuals is constant
lmtest::bptest(fixed)
#p-value less than 0.05 so we can reject null and conclude heteroscedacity is present
#OLS does not work with heteroscedacity #https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/

#Test which model is better
pFtest(fixed, ols)
phtest(fixed, random)
phtest(fixed, between)
i#Fixed is better than random and between as null rejected - However, r2 higher with RE and between

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

#Fit simple linear model on aggregated dataset
agg_data <- select(read_csv("data/aggregated_data.csv"),-1)
lm_agg <- lm(`Life expectancy`~ ., data = agg_data)
sum_lm_agg <- stargazer(lm, type="text", title = "Linear regression model on aggregated data + happiness data - Summary statistics", out = "Results/Linear regression statistics.txt")
lm_agg_preds <- predict(lm_agg)
lm_agg_residuals <- as.list.data.frame(residuals(lm_agg))

#Plots of residuals vs. fitted values + predicted vs. fitted values

qqnorm(ols_residuals)
qqline(ols_residuals)

plot(agg_data$`Life expectancy`, lm_agg_preds, 
       ylab="Predicted", xlab="actual values", 
       main="Predicted vs. actual values for Linear regression model") 

plot(agg_data$`Life expectancy`, lm_agg_residuals, 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for Linear regression model") 

plot(ols_preds, ols_residuals, 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for OLS model") 

plot(Y, predict(ols), 
     ylab="Predicted", xlab="actual values", 
     main="Predicted vs. actual values for OLS model") 

plot(Y, resid(ols2), 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for OLS model with omitted variables") 

plot(agg_data$`Life expectancy`, lm_res, 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for Linear model") 

plot(Y, resid(fixed), 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for Fixed effects model") 

plot(Y, predict(fixed), 
     ylab="Predicted", xlab="actual values", 
     main="Predicted vs. actual values for Fixed effects model") 

plot(Y, resid(fixed2), 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for Fixed effects model with omitted variables") 

plot(Y, predict(fixed2), 
     ylab="Predicted", xlab="actual values", 
     main="Predicted vs. actual values for Fixed effects model with omitted variables")

plot(Y, resid(random), 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for random effects model") 

plot(Y, predict(random), 
     ylab="Predicted", xlab="actual values", 
     main="Predicted vs. actual values for random effects model") 

plot(Y, resid(random2), 
     ylab="Residuals", xlab="predicted values", 
     main="Residuals vs. predicted values for random effects model with omitted variables") 

plot(Y, predict(random2), 
     ylab="Predicted", xlab="actual values", 
     main="Predicted vs. actual values for random effects model with omitted variables")

