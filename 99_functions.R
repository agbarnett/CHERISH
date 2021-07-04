## 99_functions.R
# common functions for markdown documents
# June 2020

Missing = function(x) base::sum(is.na(x))
Mean = function(x) base::mean(x, na.rm=TRUE)
Median = function(x) stats::quantile(x, probs=0.5, na.rm=TRUE)
Q1 = function(x) stats::quantile(x, probs=0.25, na.rm=TRUE)
Q3 = function(x) stats::quantile(x, probs=0.75, na.rm=TRUE)
Min = function(x) base::min(x, na.rm=TRUE)
Max = function(x) base::max(x, na.rm=TRUE)
Sum = function(x) base::sum(x, na.rm=TRUE)
SD = function(x) stats::sd(x, na.rm=TRUE)
N = function(x) base::length(x)

# function to round with trailing zeros
roundz  = function(x, digits=0){formatC( round( x, digits ), format='f', digits=digits)}

# rename
nice_rename = function(x, scramble=FALSE){
  y = case_when(
    x == 'fromd_dc_index_team' ~ 'Post discharge',
    x == 'fromd_unpl_adm1_m6' ~ 'Post readmission',
    x == 'I(from == "d_adm1_m6")TRUE' ~ 'Post readmission',
    x == 'I(from == "d_unpl_adm1_m6")TRUE' ~ 'Post readmission',
    x == 'age.c*age.c' ~ 'Age squared',
    x == 'age2' ~ 'Age squared',
    x == 'I((age - 76)/10)' ~ 'Age (+10 years)',
    x == 'age_c' ~ 'Age (+10 years)',
    x == 'age.c' ~ 'Age (+10 years)',
    x == 'spmsq_score_admission.c' ~ 'SPMSQ (-3)',
    x == 'spmsq_score_admission_c' ~ 'SPMSQ (-3)',
    x == 'I((spmsq_score_admission - 8)/(-3))' ~ 'SPMSQ (-3)',
    x == 'I((cci_unadjusted_for_age - 2)/2)' ~ 'Charlson (+2)',
    x == 'cci_unadjusted_for_age.c' ~ 'Charlson (+2)',
    x == 'cci_unadjusted_for_age_c' ~ 'Charlson (+2)',
    x == 'intervention_factorIntervention' & scramble==FALSE~ 'Intervention',
    x == 'intervention_factorIntervention' & scramble==TRUE~ 'Intervention (scrambled)',
    x == 'intervention' & scramble==FALSE~ 'Intervention',
    x == 'intervention' & scramble==TRUE~ 'Intervention (scrambled)',
    x == 'I(gender == "Male")TRUE' ~ 'Male',
    x == 'genderFemale' ~ 'Female',
    x == 'female' ~ 'Female',
    x == 'elective' ~ 'Elective',
    x == 'adl_baseline' ~ 'ADL at baseline',
    x == 'baseline_a' ~ 'Any ADL at baseline',
    x == 'baseline_i' ~ 'Any IADL at baseline',
    x == 'I(adl_admission > 0)TRUE' ~ 'Any ADL at admission',
    x == 'I(adl_admission>0)TRUE' ~ 'Any ADL at admission',
    x == 'I(adl_baseline>0)TRUE' ~ 'Any ADL at baseline',
    x == 'I(adl_baseline > 0)TRUE' ~ 'Any ADL at baseline',
    x == 'I(iadl_baseline>0)TRUE' ~ 'Any IADL at baseline',
    x == 'source01Post-intervention' ~ 'Post-intervention period',
    x == 'intervention:elective' ~ "Intervention x Elective interaction",
    TRUE ~ as.character(x)
  )
  return(y)  
}

# function to prepare data for winbugs and run model
run_winbugs = function(indata, 
            outcome, 
            vars = c('age_c','gender','elective'),
  num.chains = 2,   # MCMC stuff
  samples = 5000,
  thin = 3){

  # does it use hospital?
  use.hosp = any('hospital'%in% vars)
  if(use.hosp==FALSE){bugs.model = 'bugs_latent_no_hospital.txt'}
  if(use.hosp==TRUE){bugs.model = 'bugs_latent.txt'}
  
  # data
  data_for_model = filter(data, !is.na(spmsq_score_admission_c)) # exclude missing
  predictors = vars[vars!='hospital'] # remove hospital from this list (done as random variable)
  n.beta = length(predictors) # 
  N = nrow(data_for_model)
  # set up outcome
  if(outcome=='ADL'){freq = model.matrix( ~ -1 + status_d30_adl, data = data_for_model)} # to get binary matrix
  if(outcome=='IADL'){freq = model.matrix( ~ -1 + status_d30_iadl, data = data_for_model)} # to get binary matrix
  # set up predictors
  X = select(indata, all_of(predictors))
  # set up bugs data 
  bdata = list(N = N, ncat = 4, n.beta = n.beta, X = as.matrix(X),
               freq = as.matrix(freq))
  if(use.hosp == TRUE){bdata$hosp = as.numeric(as.factor(data$hospital))}

  # initial values
  inits = list(C=c(NA, 1, 2), beta=rep(0, n.beta)) # initial values (two sets)
  if(use.hosp == TRUE){inits$gamma=rep(0,4); inits$tau.gamma=1}
  inits = rep(list(inits), num.chains)
  
  # run BUGS
  parms = c('C','beta')
  if(use.hosp==TRUE){parms = c(parms, 'gamma.c')}
  bugs.results =  bugs(data=bdata, inits=inits, parameters=parms, model.file=bugs.model, DIC=TRUE,
                       n.chains=num.chains, n.iter=samples*2*thin, n.thin=thin, debug=FALSE,
                       bugs.directory="c:/Program Files/WinBUGS14")
  # neaten estimates
  ests = data.frame(bugs.results$summary[,c(1,3,7)]); names(ests) = c('mean','lower','upper')
  ests$var = row.names(ests)
  ests = filter(ests, str_detect(var, '^beta')) %>% # just betas #
    mutate(variable = predictors) %>%
    filter(variable != 'intercept') %>%
    mutate(mean = exp(mean), # convert to odds ratios
           lower = exp(lower),
           upper = exp(upper),
           variable = nice_rename(variable, scramble=scramble),
           mean = roundz(mean, 2),
           CI = paste(roundz(lower, 2), ' to ', roundz(upper,2), sep='')) %>%
    select(variable, mean, CI)
  
  # to return
  return = list()
  dic.frame = data.frame(outcome=outcome, pD=bugs.results$pD, DIC=bugs.results$DIC)
  return$dic = dic.frame
  return$max_rhat = max(bugs.results$summary[,8]) # maximum R-hat
  return$ests = ests
  return(return)
} 


###
## cox models of six month mortality
six_month = function(indata, 
                     imputed, # list of imputed data sets
                     ylab ='',
                     compete = '', # time of competing risk (if any)
                     time = 'days_from_unit_dc_to_death', # name of time variable
                     outcome = 'death_within_6mths'  # binary outcome (assume 1 is event)
                     ){
  # rename time and outcome
  index = grep(outcome, names(indata))
  names(indata)[index] = 'outcome'
  index = grep(time, names(indata))
  names(indata)[index] = 'time'
  
  # loop through imputations
  betas = vars = NULL # means and variances for imputed results
  for (k in 1:5){ # loop through imputations
    data.for.model = dplyr::select(indata, -iadl_baseline, -spmsq_score_admission) # remove missing variables
    data.for.model = left_join(data.for.model, impute[[k]], by='subject_num')
    data.for.model = mutate(data.for.model, 
                            age.c = (age -76)/10, # standardised for regression
                            cci_unadjusted_for_age.c = (cci_unadjusted_for_age -2)/2, # standardised for regression
                            spmsq_score_admission.c = (spmsq_score_admission -8)/(-3)) # standardised for regression (recalculate)
    # Cox model
    cox_model = coxph(Surv(time, outcome==1) ~ intervention + age.c + I(gender=="Male") + cci_unadjusted_for_age.c + spmsq_score_admission.c + I(adl_admission>0) + elective + hospital, data=data.for.model)
    betas[[k]] = cox_model$coefficients
    vars[[k]] = cox_model$var
  }
  mi.res = summary(MIcombine(betas, vars)) # combine multiple imputations
  mi.res$term = row.names(mi.res)
  ests = mutate(mi.res,
        term = nice_rename(term, scramble=FALSE),
        HR = exp(results),
           conf.low = exp(`(lower`),
           conf.high = exp(`upper)`),
           HR = roundz(HR, 2),
           CI = paste(roundz(conf.low,2), ' to ', roundz(conf.high,2), sep='')) %>%
    select(term, HR, CI)
  
  # K-M plot
  km = survfit(Surv(time, outcome==1) ~ intervention_factor, data=indata)
  g = ggsurvplot(km, data=indata)
  # alternative plot
  colours = c('darkseagreen2','lightgoldenrod2')
  km_plot = ggplot(g$data.survplot, aes(x=time, y=1-surv, ymin = 1-lower, ymax=1-upper, col=intervention_factor, fill=intervention_factor))+
    scale_fill_manual(NULL, values=colours)+
    scale_color_manual(NULL, values=colours)+
    geom_ribbon(alpha=0.5)+
    geom_step(size=1.05)+
#    scale_y_continuous(limits=c(0,1))+
    xlab('Days since unit discharge')+
    ylab(ylab)+
    scale_x_continuous(breaks=seq(0,180,30))+
    theme_bw()+
    theme(
      legend.position = c(0.8, 0.2),
      panel.grid.minor= element_blank())

  # return
  to_return = list()
  to_return$model = cox_model
  to_return$ests = ests
  to_return$plot = km_plot
  return(to_return)
}


### set up data for competing risks and run cumulative incidence curves
competing_risks_times = function(indata,
                                 dates = c('d_dc_index_team','death_date','d_adm1_m6'), # dates of interest
                                 censor_day = 183){
# exclude in-hospital deaths:
for.survival = filter(indata,
                      dc_dest != 'Deceased') 

### Section 1: make data ###
## possible transitions
dates_long = select(for.survival, 'subject_num', all_of(dates)) %>%
  mutate(start = d_dc_index_team, # replicate start date
         censored = d_dc_index_team + censor_day) %>% # add censored date
  pivot_longer(!c(subject_num, start), names_to='event', values_to='date') %>% # start date on every row
  filter(!is.na(date)) %>% # remove empty dates
  arrange(subject_num, date) %>%
  mutate(time = as.numeric(date - start)) %>% # get time difference from discharge (start date)
  filter(time <= censor_day) # exclude events after six months

# now loop through patients to get one step ahead times
time_dep_data = NULL
for (u in unique(dates_long$subject_num)){
  this_patient = filter(dates_long, subject_num == u)
  n = nrow(this_patient)
  for (r in 1:(n-1)){
    f = data.frame(subject_num = u, 
                   from = this_patient$event[r], 
                   to = this_patient$event[r+1], 
                   entry = this_patient$time[r], 
                   exit = this_patient$time[r+1],
                   entry_date = this_patient$date[r], # add dates for joint models
                   exit_date = this_patient$date[r+1])
    time_dep_data = bind_rows(time_dep_data, f)
  }
}
# exclude transitions:
time_dep_data = filter(time_dep_data, !from=='death_date') # remove transitions from death, occurred because of censoring dates
with(time_dep_data, table(from, to))

# random check
u = sample(time_dep_data$subject_num, 1)
filter(time_dep_data, subject_num == u)
filter(dates_long, subject_num == u)

# merge back patient data
time_dep_data = left_join(time_dep_data, for.survival, by='subject_num') %>%
  mutate(clock_reset = exit-entry +0.5) # clock reset for delayed entries

## Section 2: run curves
# separate curves for randomised groups
cum_inc0 = with(filter(time_dep_data, intervention==0), cuminc(ftime=clock_reset, fstatus=to, group=from, cencode='censored'))
cum_inc1 = with(filter(time_dep_data, intervention==1), cuminc(ftime=clock_reset, fstatus=to, group=from, cencode='censored'))
# remove lines not needed (e.g., stroke to stroke)
if(length(cum_inc0) > 1){
  any_ests0 = any_ests1 = NULL
  for (z in 1:(length(cum_inc0)-1)){ # minus 1 as last list is not a curve
    empty0 = any(cum_inc0[[z]]$est !=0) # find missing
    any_ests0 = c(any_ests0, empty0)
  }
  for (z in 1:(length(cum_inc1)-1)){
    empty1 = any(cum_inc1[[z]]$est !=0) # find missing
    any_ests1 = c(any_ests1, empty1)
  }
  index0 = rev(which(any_ests0 == FALSE))
  index1 = rev(which(any_ests1 == FALSE))
  for (z0 in index0){
    cum_inc0[[z0]] = NULL # blank lines
  }
  for (z1 in index1){
    cum_inc1[[z1]] = NULL # blank lines
  }
}
g0 = ggcompetingrisks(cum_inc0,
                      gsep=' ')
g1 = ggcompetingrisks(cum_inc1,
                      gsep=' ')
# make new plot by combining two groups
for_plot = bind_rows(g0$data, g1$data, .id='intervention') %>%
  mutate(
    intervention = ifelse(intervention==1, 'Control', 'Intervention'),
    event_num = ifelse(event=='death_date', 1, 2),
    facet_num = ifelse(group=='d_dc_index_team', 1, 2),
    facet_factor = factor(facet_num, levels=1:2, labels=c('Post discharge','Post readmission'))) 

# return
to_return = list()
to_return$for_plot = for_plot
to_return$data = time_dep_data

return(to_return)

}