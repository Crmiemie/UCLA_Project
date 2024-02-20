################################################################################
# Project1 PartA: An Example of Turning A Small Dataset Into Knowledge
# By Chaoran Yang
# updated on 12/2/2024
################################################################################
library(olsrr)
library(readxl)
library(ggplot2)
library(car)

Dir = "/Users/yangchaoran/Desktop/course document/Data"
outputDir = paste0(Dir, "/Output")
setwd(Dir)

# Import the corporatetax data
corporatetax_data = data.frame(read_excel("P02_Corporate tax.xlsx"))
str(corporatetax_data)

# Equation 1
linear_model1 = lm(corporatetax_data$ypcg ~ corporatetax_data$ctax, data = corporatetax_data)
summary(linear_model1)
# Equation 2
linear_model2 = lm(ypcg ~ ctax + ypc2000, data = corporatetax_data)
summary(linear_model2)
# Equation 3
linear_model3 = lm(ypcg ~ ctax + ypc2000 + dty + (ctax*dty), data = corporatetax_data)
summary(linear_model3)

# predict a hypothetical GDP per capita growth rate besed on Equation 3
pred_model3 = predict(linear_model3, newdata = data.frame(ctax = 20, ypc2000 = 10000, dty = 35))
pred_model3         

# Plot a chart similar to Figure 4
plot(corporatetax_data$ctax, corporatetax_data$ypcg, pch = 19, col = "blue",
     xlab = "Average Corporate Tax Rate '00-'08(%)", ylab = "Average GDP per capita Growth '00-'15(%)",
     xlim = c(10, 45), ylim = c(-1, 6), xaxt = "n", yaxt = "n")
abline(linear_model1, col = "red", lwd = 2); abline(h = 0)
axis(1); axis(2,las = 1)
grid(lty = 1)
title(main = "The association between the average corporate tax rate '00-'08 \n and the average GDP growth per capita '00-'15")

# Think in the Next: Why do I use corporate tax rates averaged from 2000 to 2008 instead of from 2000 to 2015?
"
 I think it's because the years 2000 to 2008 represent a period of relative economic stability and growth before the global financial crisis hit in 2008, so it's better to analyze the relationship between corporate tax rates and economic growth during a time when the economy was not significantly influenced by the recession.
 In a nut shell, this allows for a clearer examination of the impact of corporate tax rates on economic growth without the confounding effects of a major economic downturn.
"

# Select the best model
linear_model = lm(ypcg ~ . - country, data = corporatetax_data)
best_subset = ols_step_best_subset(linear_model)
best_subset
plot(best_subset)
write.csv(as.data.frame(best_subset), "Model_Selection_Report.csv", row.names = FALSE) # Based on Adj.R-Square, the model using 6 predictors are the best

# Get the best model
summary(linear_model)
write.csv(as.data.frame(linear_model$coefficients), "Best_Model.csv")

# Conclusion 
"
In terms of Adjusted-R square values, the model uses ctax, ypc2000, dty, trade, ihc, and y2000 (6 predictors) is the best
The equation is: ypcg = 2.066 - 0.079 * ctax - 0.0000373 * ypc2000 - 0.0065 * dty + 0.0065 * trade + 1.038 * ihc + 0.000102 * y2000
"

################################################################################
# Project1 PartB: Multiple Linear Regression with Categorical Variables
# By Chaoran Yang
# updated on 12/2/2024
################################################################################
# Import the obesity data
obesity_data=data.frame(read_csv("/Users/yangchaoran/Desktop/course document/Data/Obesity.csv"))
str(obesity_data)
colname(oebesity_data)
# Create a new variable BMI
obesity_data$BMI <- obesity_data$Weight / (obesity_data$Height * obesity_data$Height)
head(obesity_data)

# Run linear regression with BMI as the dependent variable
model <- lm(BMI ~ ., data = obesity_data)
summary(model)
vif(model)

# Briefly explain the results
"
1.Interpretation of Coefficients:
The coefficients represent the change in the dependent variable (BMI) for a one-unit change in the corresponding explanatory variable, holding other variables constant.
For instant, the coefficient for 'Weight' is 0.2603, indicating that for every one-unit increase in weight, the BMI increases by 0.2603 units, holding other variables constant.

2.Statistical Significance
Variables with p-values less than the significance level (commonly 0.05) are considered statistically significant predictors of BMI.
For instant, varibles like GenderMale, Height, Weight and family_history_with_overweightyes have statistically significant coefficients and are likely to be important predictors of the BMI in the model.

3.Adjusted R-squared
The adjusted R-squared value indicates the proportion of variance in BMI explained by the explanatory variables in the model.
The high value (0.9949) suggests that the model fits the data well.

4.Multicollinearity Problem
VIF values greater than 5 indicate multicollinearity among the explanatory variables.
Some variables, such as 'Weight' and 'Height' have high VIF values, suggesting multicollinearity issues.

"

# Modify the model by dealing with Multicollinearity problem and removing some varibles that are not significant
refined_model <- lm(BMI ~ Gender + Weight + FAVC + NCP + CAEC + SMOKE + CH2O + TUE + MTRANS + NObeyesdad, data = obesity_data)
summary(refined_model)
vif(refined_model) # It's better now maybe



