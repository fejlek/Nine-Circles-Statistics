# Nine Circles of Statistical Modeling

The primary focus of this repository is *statistical models*, i.e., models based on more or less restrictive assumptions about the data-generating process, such as linear regression, Poisson regression, or quantile regression (I plan to cover 9 regression models at the time of writing this introduction). I will demonstrate their use on datasets from Kaggle (https://www.kaggle.com) and other sources, such as various R packages. Even though each problem will mainly focus on one particular model, since we are dealing with more or less realistic datasets, I will have to cover additional topics as the need arises, e.g., missing data (multiple imputation) and dependent/correlated observations.

The main motivation behind this work is personal: learning, practice, future reference, and, last but not least, fun. Still, I think that these small projects can be useful for any reader interested in statistical modeling. Kaggle users usually provide their solutions using machine learning techniques, so these solutions can be somewhat unique, at least in this regard.
## Regression modeling strategies

In the solutions provided here, I will (or at least try to) follow these steps to obtain valid statistical inference and models that perform well overall (inspired by *F. Harrell, Regression modeling strategies. New York: Springer-Verlag, 2001*).

1. Data exploration (checking for missing/nonsensical values of predictors, redundancy analysis of predictors, elimination and/or grouping of predictors if needed)
2. Formulating hypotheses and the corresponding full model (main linear effects, nonlinear effects, interactions) based on the nature of the problem and effective sample size (rule of thumb: 10-20 independent observations per parameter). We should not use the predicted values (formally or informally) in model selection.
3. Single or multiple imputation (rule of thumb: multiple imputation should be used if the proportion of observations with missing values is greater than 3%). We should use a multiple imputation model that is at least as general as our full model.
4. Fitting a full model.
5. Very limited model simplification by testing the significance of *all* interaction and/or *all* nonlinear terms (rule of thumb: provided that the corresponding statistics have a P-value greater than 0.2, it should be safe to simplify the model by deleting all corresponding terms). Other than that, we should not remove any seemingly nonsignificant effects from the model. This step should be skipped if we are interested only in hypothesis testing: a full model fit will yield more accurate P-values for tests of the variables of interest.
6. Checking the distributional assumptions (e.g., by analyzing the residuals) and the presence of overly influential observations. We change the model if needed to obtain valid inference (i.e., we must return to step 4).
7. Model interpretation, effect estimation, hypothesis testing, and model performance measures. 
8. 1. Model validation (of predictive accuracy) via bootstrap or cross-validation. If we performed multiple imputation, we should include step 3 in the validation. If we performed variable selection beyond what is suggested in 5, we should include step 5 in the validation. This step is not strictly necessary if we are only interested in hypothesis testing and/or effect estimation rather than prediction. However, the effect estimation and hypothesis testing presented here are model-based; thus, it is desirable to ascertain that the model used to draw the conclusions is reasonable (namely, to assess the degree of overfitting).  

## Technical solution

All solutions are programmed in R and provided as Markdown documents.

## Table of Contents (main & additional topics)

1. Linear regression (heteroskedasticity and correlated observations; heteroskedasticity/cluster robust standard errors; random effects models; introduction to various bootstrap methods and cross-validation)
2. Logistic regression (dealing with missing data; multiple imputation; discrimination measures and calibration for binary data; decision curve analysis)
3. Ordinal regression (proportional odds, continuation ratio, and adjacent categories models; partial ordinal models; discrimination measures for ordinal data)
4. Count regression (Poisson and negative binomial models; hurdle and zero-inflated models; discrimination measures for count data)
5. Survival analysis (Kaplan-Meier estimator; Cox proportional hazards model; accelerated failure time models; discrimination measures and calibration for survival data)
6. Extreme value analysis (block maxima (GEV), threshold excess (GP), and Poisson point process models; methods for non-stationary time series: time-varying parameters and the mean & variance trend approach)
7. Quantile regression (common models for skewed response: log-linear model; gamma, inverse Gaussian, and Tweedie)
8. Generalized additive models
9. LASSO (Bayesian LASSO; adaptive LASSO; double-selection LASSO; ridge regression; post-selection inference)

