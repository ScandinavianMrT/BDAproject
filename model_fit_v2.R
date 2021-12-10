library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(plm)
library(janitor)
library(stargazer)
library(ggplot2)
library(openxlsx)

data <- read_csv("data/FinalDF.csv")
df <- data %>%
  janitor::clean_names()

pdf <- pdata.frame(df, index = c("country", "year"))
attach(pdf)
  
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
  
  
  cols <- c("adult_mortality","infant_deaths","alcohol","percentage_expenditure",
                    "hepatitis_b","measles","bmi","under_five_deaths","polio","total_expenditure",
                    "thinness_5_9_years","income_composition_of_resources","schooling",
                    "emission","military_expenditure")
  
  RMSE = function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  #Fit ols model and inspect residuals
  ols <- plm(Y ~ X, data = pdf, model = "pooling")
  sum_ols <- stargazer(ols, type="text", title = "OLS model - full data - Summary statistics", out = "Results/OLS model - full data - Summary statistics.txt")
  pdf$ols_preds <- as.list.data.frame(predict(ols))
  pdf$ols_residuals <- as.list.data.frame(residuals(ols))
  ols_error <- RMSE(pdf$ols_preds, pdf$life_expectancy)
  #Omit correlated variables
  ols2 <- plm(Y ~ X2, data = pdf, model = "pooling")
  sum_ols2 <- stargazer(ols2, type="text", title = "OLS model - omitted variables - Summary statistics", out = "Results/OLS model - omitted variables - Summary statistics.txt")
  pdf$ols2_preds <- as.list.data.frame(predict(ols2))
  pdf$ols2_residuals <- as.list.data.frame(residuals(ols2))
  ols2_error <- RMSE(pdf$ols2_preds, pdf$life_expectancy)
  
  #fit fixed effect model - Subtracts mean of group for each group to isolate variations within group
  fixed <- plm(Y ~ X, data = pdf, model = "within")
  sum_fixed <- stargazer(fixed, type="text", title = "Fixed effects model - Full data - Summary statistics", out = "Results/Fixed effects - Full data - Summary statistics.txt")
  #Calculate "manually" because model.response returns demeaned values
  fixed_preds <- as.data.frame(as.numeric(fixed$model[[1]] - fixed$residuals))
  pdf$fixed_preds <- fixed_preds[,1]
  pdf$fixed_residuals <- as.list.data.frame(residuals(fixed))
  fixed_error <- RMSE(pdf$fixed_preds, pdf$life_expectancy)
  #Omit correlated variables
  fixed2 <- plm(Y ~ X2, data = pdf, model = "within")
  sum_fixed2 <- stargazer(fixed2, type="text", title = "Fixed effects - omitted variables - Summary statistics", out = "Results/Fixed effects - omitted variables - Summary statistics.txt")
  fixed2_preds <- as.data.frame(as.numeric(fixed2$model[[1]] - fixed2$residuals))
  pdf$fixed2_preds <- fixed2_preds[,1]
  pdf$fixed2_residuals <- as.list.data.frame(residuals(fixed2))
  fixed2_error <- RMSE(pdf$fixed2_preds, pdf$life_expectancy)
  
  #fit random effect model
  random <- plm(Y ~ X, data = pdf, model = "random")
  sum_random <- stargazer(random, type="text", title = "Random effects model - omitted variables - Summary statistics", out = "Results/Random effects model - Full data - Summary statistics.txt")
  #Calculate "manually" because model.response returns demeaned values
  random_preds <- as.data.frame(as.numeric(random$model[[1]] - random$residuals))
  pdf$random_preds <- random_preds[,1]
  pdf$random_residuals <- as.list.data.frame(residuals(random))
  random_error <- RMSE(pdf$random_preds, pdf$life_expectancy)
  #Omit correlated variables
  random2 <- plm(Y ~ X2, data = pdf, model = "random")
  sum_random2 <- stargazer(random2, type="text", title = "Random effects model - omitted variables - Summary statistics", out = "Results/Random effects model - omitted variables - Summary statistics.txt")
  random2_preds <- as.data.frame(as.numeric(random2$model[[1]] - random2$residuals))
  pdf$random2_preds <- random2_preds[,1]
  pdf$random2_residuals <- as.list.data.frame(residuals(random2))
  random2_error <- RMSE(pdf$random2_preds, pdf$life_expectancy)
  
  errors <- write.xlsx(data.frame(ols_error, ols2_error, fixed_error,fixed2_error,random_error, random2_error), "Results/RMSE.xlsx")
  
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
  stargazer(lm_agg, type="text", title = "Linear regression model on aggregated data + happiness data - Summary statistics", out = "Results/Linear regression statistics.txt")
  lm_agg_preds <- predict(lm_agg)
  lm_agg_residuals <- as.list.data.frame(residuals(lm_agg))
  
  #Check if residuals are normally distributed
  shapiro.test(pdf$ols_residuals)
  shapiro.test(pdf$fixed_residuals)
  shapiro.test(pdf$random_residuals)
  shapiro.test(pdf$ols2_residuals)
  shapiro.test(pdf$fixed2_residuals)
  shapiro.test(pdf$random2_residuals)
  shapiro.test(lm_agg_residuals) # p is somewhat significant
  #All models, except lm, have p < 0.0001 so we can conclude residuals are non-normal across the board

#Plots 

boxplot(pdf[cols], main="Value distributions for Life Expectancy dataset", las=2)

plot(agg_data$`Life expectancy`, lm_agg_preds, 
       ylab="Predicted", xlab="Actual values", 
       main="Predicted vs. actual values for Linear regression model") 

plot(fitted(lm_agg), lm_agg_residuals, 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. fitted values for Linear regression model") 

qqnorm(lm_agg_residuals) +
qqline(lm_agg_residuals)

plot(as.list.data.frame(predict(ols)),as.list.data.frame(residuals(ols)), 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. Fitted values for OLS model") 

plot(Y, pdf$ols_preds, 
     ylab="Predicted", xlab="Actual values", 
     main="Predicted vs. actual values for OLS model") 

qqnorm(pdf$ols_residuals) +
qqline(pdf$ols_residuals)

plot(as.list.data.frame(predict(ols2)),as.list.data.frame(residuals(ols2)), 
     ylab="Residuals", xlab="Predicted values", 
     main="Residuals vs. predicted values for OLS model with omitted variables") 

qqnorm(pdf$ols2_residuals) +
qqline(pdf$ols2_residuals)

plot(as.list.data.frame(pdf$fixed_preds), as.list.data.frame(pdf$fixed_residuals), 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. fitted values for Fixed effects model") 

plot(Y, pdf$fixed_preds,
     ylab="Predicted", xlab="Actual values", 
     main="Predicted vs. Actual values for Fixed effects model") 

qqnorm(pdf$fixed_residuals) +
qqline(pdf$fixed_residuals)

plot(as.list.data.frame(pdf$fixed2_preds), as.list.data.frame(pdf$fixed2_residuals), 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. Fitted values for Fixed effects model with omitted variables") 

plot(Y, predict(fixed2), 
     ylab="Predicted", xlab="Actual values", 
     main="Predicted vs. actual values for Fixed effects model with omitted variables")

qqnorm(pdf$fixed2_residuals) +
qqline(pdf$fixed2_residuals)

plot(as.list.data.frame(pdf$random_preds), as.list.data.frame(pdf$random_residuals), 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. fitted values for random effects model") 

plot(Y, as.list.data.frame(pdf$random_preds), 
     ylab="Predicted", xlab="Actual values", 
     main="Predicted vs. actual values for random effects model") 

qqnorm(pdf$random_residuals) +
qqline(pdf$random_residuals)

plot(as.list.data.frame(pdf$random2_preds), as.list.data.frame(pdf$random2_residuals), 
     ylab="Residuals", xlab="Fitted values", 
     main="Residuals vs. fitted values for random effects model with omitted variables") 

plot(Y, as.list.data.frame(pdf$random2_preds), 
     ylab="Predicted", xlab="Actual values", 
     main="Predicted vs. actual values for random effects model with omitted variables")

qqnorm(pdf$random2_residuals) +
qqline(pdf$random2_residuals)
                 