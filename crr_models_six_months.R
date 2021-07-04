# crr_models_six_months.R
# run cumulative risk regression
# feb 2021

# formula with fixed hospital effect
formula = as.formula('dummy ~ intervention + age.c + I(gender=="Male") + cci_unadjusted_for_age.c + spmsq_score_admission.c + I(adl_admission>0) + elective + hospital')

## run models with imputed data
coef = vcov = NULL
for (k in 1:N.impute){ # loop through imputed data
  # get imputed data and standardise
  this_data = select(data_for_surv, -iadl_baseline, -spmsq_score_admission) %>% # remove missing variables
    left_join(impute[[k]], by='subject_num') %>%
    mutate(dummy = 1, # for formula
           age.c = (age -76)/10, # standardised for regression
           age2 = as.numeric(age>80)*(age-80)/10, # knot - did not help
           cci_unadjusted_for_age.c = (cci_unadjusted_for_age -2)/2, # standardised for regression
           spmsq_score_admission.c = (spmsq_score_admission-8)/(-3)) # minus median, 
  # design matrix (because of categorical variables):
  M = model.matrix(formula, data=this_data) # complete data from MICE imputation
  cov1 = as.matrix(M[,-1]) # exclude intercept
  # model
  cum_rr = with(this_data, crr(ftime=clock_reset, fstatus=to==outcome, cov1=cov1, cencode='censored', maxiter = 100))
  # add estimates
  coef[[k]] = cum_rr$coef
  vcov[[k]] = cum_rr$var
}
# get combined estimates across all imputations
crr = summary(MIcombine(coef, vcov)) %>% mutate(Outcome = outcome )
crr$var = names(cum_rr$coef)
to_table = mutate(crr,
                  var = nice_rename(var, scramble=FALSE),
         HR = roundz(exp(results), 2),
         lower = roundz(exp(`(lower`), 2),
         upper = roundz(exp(`upper)`), 2),
         CI = paste(lower, ' to ', upper, sep='')) %>%
  select(var, HR, CI)
