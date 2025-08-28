# World-Happiness-Index-Indicator

# Summary
This project analyzes the World Happiness Index dataset (2020) from Kaggle using R. The goal is to understand the key drivers of happiness across countries and build predictive models to classify countries into “Happy” vs. “Unhappy” categories.

# Tools Used
- R (ggpubr, tidyverse, readxl, ggplot2,dplyr,randomForest) 
- Data Preparation: Exploratory Data Analysis, Factor encoding, train-test split (80/20) 
- Evaluation: ROC Curve, AIC, AUC, Misclassification error

# Model
1. Logistic Regression
Model 1: Happiness ~ GDP per capita
Model 2 (Full): Included all predictors.
Stepwise Regression: Removed less significant variables (e.g., GDP dropped in stepwise due to multicollinearity).
Model performance evaluated with:
AIC values
ROC curves & AUC scores
Confusion matrices under different probability cutoffs

2.  Cross-Validation
10-fold cross-validation applied to reduce overfitting.
Custom cost function tested misclassification errors.

3.  Random Forest
- Built multiple random forest models with different mtry values.
- Optimized using Out-of-Bag (OOB) error rates.
- Variable importance plots showed Social Support and Life Expectancy as the strongest predictors.
- The Final Random Forest model achieved the lowest misclassification error compared to logistic regression.

# Key Findings
- Social Support and Healthy Life Expectancy are the strongest predictors of happiness.
- Random Forest outperformed logistic regression in predictive accuracy.
- Logistic regression was useful for interpretation, while Random Forest provided robustness.
- Optimal probability cutoff for logistic regression was 0.5, balancing false positives and false negatives.

