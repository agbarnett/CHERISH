# CHERISH

R code to analyse the data from the CHERISH trial, a multi-site improvement program to reduce geriatric syndromes in older inpatients. The trial protocol is here: https://bmcgeriatr.biomedcentral.com/articles/10.1186/s12877-016-0399-7. 

The main file is the Rmarkdown file `cherish.stats.report.Rmd` which creates a complete statistical report. The report was created using R version 3.4.4. The key R libraries are: 

* `cmprsk` for competing risk survival analysis based on length of stay in hospital and whether the patient was dischared or died.

* `geepack` to run the logistic regression models that examine binary outcomes adjusting for within-patient correlation.

* `MICE` to impute a small amount of missing data for two predictors (`impute.mice.R`).

* `mitools` to combine the results based on multiply imputed data.

The key outcome of length of stay was estimated using a Bayesian Weibull survival model in JAGS (version 4.2). The model files are `survival.bugs.primary.txt` and `survival.bugs.primary.time.txt` for the secondary analysis that examines an interaction between the intervention and time. This interaction uses fractional polynomials to model non-linear shapes over time (`fractional.polynomial.R`). The data were prepared for JAGS using `prepare.jags.R` and the results processed using `prepare.jags.results.R`.

The data are confidential and cannot be openly shared.
