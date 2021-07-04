# cox_models_six_months.R
# cox models for six months outcomes, called by cherish.stats.report.updates.Rmd
# works for either readmission
# using multiple imputation
# Feb 2021

# formula with random intercept for hospital
formula = as.formula('surv ~ intervention + age.c + I(gender=="Male") + cci_unadjusted_for_age.c + spmsq_score_admission.c + I(adl_admission>0) + elective + f(hospital, model = "iid")')

# loop through imputations
N.impute = 5
model = list() # for storing imputed models
residuals = NULL
for (k in 1:N.impute){ # loop through imputations
  # get imputed data and standardise
  this_data = select(data_for_surv, -iadl_baseline, -spmsq_score_admission) %>% # remove missing variables
    left_join(impute[[k]], by='subject_num') %>%
    mutate(age.c = (age -76)/10, # standardised for regression
           age2 = as.numeric(age>80)*(age-80)/10, # knot
           cci_unadjusted_for_age.c = (cci_unadjusted_for_age -2)/2, # standardised for regression
           spmsq_score_admission.c = (spmsq_score_admission-8)/(-3)) # minus median, 
  # run models 
  surv <- with(this_data, inla.surv(time=clock_reset, event = as.numeric(to==outcome)))
  #death_weib <- inla(formula, data = this_data, family = "weibull.surv", control.compute = list(dic = TRUE)) # did not converge
  model_cox <- inla(formula, data = this_data, family = "coxph", control.compute = list(dic = FALSE))
  model[[k]] = model_cox
  # residuals
  res = bri.surv.resid(inla.obj = model_cox, time=this_data$clock_reset, event=as.numeric(this_data$to==outcome))
  res$family = NULL
  small = select(this_data, subject_num, intervention, age, cci_unadjusted_for_age, spmsq_score_admission, elective, hospital, from, to)
  r_df = data.frame(res) %>% bind_cols(small) %>% # add select data
    mutate(k=k, model = outcome)
  residuals = bind_rows(residuals, r_df)
} # end of imputation loop
# now average models using BMA (see https://becarioprecario.bitbucket.io/inla-gitbook/ch-missing.html#sec:misscovar)
model.imp <- inla.merge(model, rep(1 / N.impute, N.impute))
# neat estimates
ests = mutate(model.imp$summary.fixed,
              vars = rownames(model.imp$summary.fixed)) %>%
  mutate(vars = nice_rename(vars, scramble=FALSE),
              z = qnorm(0.975),
              lower = mean - (z*sd), # make CI
              upper = mean + (z*sd),
              model = outcome) %>%
  select(-z)
# 
