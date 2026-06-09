## =========================================================================
## Project: Statistical Analysis of Body Composition & Health Indicators
## Author: Rasha Quadri
## Date: 2026-03-22
## Description: Comprehensive statistical modeling suite analyzing relationships 
##              between body composition metrics and metabolic indicators using 
##              Linear Regression, Multiple Regression, and ANOVA methodologies.
## =========================================================================

## --- Load Required Libraries ---
library(car)
library(lmtest)
library(ggplot2)


## 1. DATA PREPROCESSING ---------------------------------------------------

# Note: Ensure 'medicaldata2.csv' is present in your active R working directory
medical <- read.csv("medicaldata2.csv", header = TRUE)
head(medical)

## Converting Target Categorical Variables to Structured Factors
medical$Gallstone_Status <- factor(medical$Gallstone_Status)
medical$CAD <- factor(medical$CAD)
medical$Diabetes_Mellitus <- factor(medical$Diabetes_Mellitus)

# Structuring Ordered Factors for Ordinal Variables
medical$Comorbidity <- factor(
  medical$Comorbidity,
  ordered = TRUE,
  levels = c(0, 1, 2, 3)
)

medical$Hepatic_Fat_Accumulation <- factor(
  medical$Hepatic_Fat_Accumulation,
  ordered = TRUE,
  levels = c(0, 1, 2, 3)
)

medical$Age_Class <- factor(
  medical$Age_Class,
  ordered = TRUE,
  levels = c("Younger", "Lower Middle", "Upper Middle", "Elder")
)


## 2. SIMPLE LINEAR REGRESSION ---------------------------------------------
## Relationship: Body Protein Content vs Total Body Fat Ratio

# Exploratory Bivariate Scatterplot
plot(
  medical$Body_Protein_Content,
  medical$Total_Body_Fat_Ratio,
  xlab = "Body Protein Content",
  ylab = "Total Body Fat Ratio",
  main = "Body Fat Ratio vs Body Protein Content"
)

# Fit Simple Linear Regression Framework
model1 <- lm(Total_Body_Fat_Ratio ~ Body_Protein_Content, data = medical)
summary(model1)

## Model Diagnostics
# Residuals vs Fitted Plot (Homoscedasticity Check)
plot(model1$fitted.values, model1$residuals,
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Normal Q-Q Plot (Normality Check)
qqnorm(model1$residuals)
qqline(model1$residuals, col = "red")

# Shapiro-Wilk Test for Residual Normality
shapiro.test(model1$residuals)

# Measure Strength of Association via Bivariate Correlation Coefficient
cor(medical$Total_Body_Fat_Ratio, medical$Body_Protein_Content)


## 3. MULTIPLE LINEAR REGRESSION -------------------------------------------
## Objective: Predicting Total Body Water Metrics

# Fit OLS Multiple Regression Framework
model2 <- lm(Total_Body_Water ~ Height + Weight + Body_Protein_Content, data = medical)
summary(model2)

## Model Diagnostics
# Residuals vs Fitted Plot
plot(model2$fitted.values, model2$residuals,
     xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Normal Q-Q Plot
qqnorm(model2$residuals)
qqline(model2$residuals, col = "red")

# Shapiro-Wilk Test for Residual Normality
shapiro.test(model2$residuals)

# Durbin-Watson Test for Residual Autocorrelation Detection
dwtest(model2)


## 4. ONE-WAY ANOVA -------------------------------------------------------
## Research Question: Evaluating Variation in Body Fat Across Age Cohorts

# Fit Analysis of Variance Model
anova_model <- aov(Total_Body_Fat_Ratio ~ Age_Class, data = medical)
summary(anova_model)

## Validation of ANOVA Modeling Assumptions
# Normal Q-Q Plot of Structural Residuals
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")

# Shapiro-Wilk Normality Test
shapiro.test(residuals(anova_model))

# Levene's Test for Homogeneity of Variance (Equal Variance Check)
leveneTest(Total_Body_Fat_Ratio ~ Age_Class, data = medical)

## Visual Comparison Matrix via Boxplot Distribution
ggplot(medical, aes(x = Age_Class, y = Total_Body_Fat_Ratio)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Total Body Fat Ratio Distribution by Age Group",
       x = "Age Cohort Category", y = "Total Body Fat Ratio") +
  theme_minimal()


## 5. TWO-WAY ANOVA WITH INTERACTION EFFECTS ------------------------------
## Research Question: Evaluating Co-dependencies of Diabetes and Hepatic Fat on Triglycerides

# Fit Two-Way ANOVA Form Factor Including Interaction Term
model3 <- aov(Triglyceride ~ Diabetes_Mellitus * Hepatic_Fat_Accumulation, data = medical)
summary(model3)

## Validation of Two-Way ANOVA Modeling Assumptions
# Shapiro-Wilk Normality Test
shapiro.test(residuals(model3))

# Normal Q-Q Plot
qqnorm(residuals(model3))
qqline(residuals(model3), col = "red")

# Levene's Test for Variance Homogeneity across Factor Combinations
leveneTest(Triglyceride ~ Diabetes_Mellitus * Hepatic_Fat_Accumulation, data = medical)

## Visualizing Factor Main Effects
# Main Effect 1: Diabetes Mellitus impact on Triglycerides
ggplot(medical, aes(x = Diabetes_Mellitus, y = Triglyceride)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightcoral", width = 0.6) +
  labs(title = "Mean Triglyceride Levels by Diabetes Status",
       x = "Diabetes Mellitus Status", y = "Mean Triglyceride Level") +
  theme_minimal()

# Main Effect 2: Hepatic Fat Accumulation impact on Triglycerides
ggplot(medical, aes(x = Hepatic_Fat_Accumulation, y = Triglyceride)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue", width = 0.6) +
  labs(title = "Mean Triglyceride Levels by Hepatic Fat Accumulation",
       x = "Hepatic Fat Accumulation Degree", y = "Mean Triglyceride Level") +
  theme_minimal()