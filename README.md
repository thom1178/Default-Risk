# Credit Card Default and Risk Scores with Logistic Regression
  In this paper, we obtain simulated credit card default data from the `ISLR` package in R. The data
contains information on a dichotomous response, credit card default, and the goal is to obtain a logistic
regression model that accurately predicts the classification of an individual. The paper obtains two logistic
regression models, one with a covariate indicating if an individual is a student and one without this
covariate. 

We explore these models and compare them with AIC, an area under the precision-recall ROC
curve, and precision-recall values based on optimized cutoff values. The final model contains only two
covariates, the balance of the loan and income of the individual. We interpret the final model in terms of
odds ratios and obtain confidence intervals for the values obtained. Finally, based on this final model we
build a Credit Risk Score that ranges from 300 to 850.

  Based on the simulated credit card default data set, we are able to accurately classify about 87% of
the testing data. This does not deviate much from sample to sample. An overall conclusion is
that student status does not supply much information to the outcome of default. This is possible
because students are primarily either in deferment or on a grace period while in school. The
biggest contributing factor to the outcome of default is the loan balance. Overwhelmingly, a higher
balance increased the odds of default over a lower balance. This effect is seen as an expected 83%
increase in odds of default for every $100 increase in balance.
