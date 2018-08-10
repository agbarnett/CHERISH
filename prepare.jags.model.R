# prepare.jags.model.R
# prepare data for primary/secondary outcome with deaths censored run in JAGS on lyra
# lyra file is run.jags.primary.R which creates results read into Rmd file
# called from cherish.stats.report.Rmd
# August 2018

# chain size (needs to be here because it applies when bugs.run is FALSE)
MCMC = 5000
thin = 3

# export all to lyra ready for JAGS
this.dir = getwd()

## loop through 5 imputes
for (k in 1:5){ #
  # add imputed data
  data.to.use = data
  data.to.use = dplyr::select(data.to.use, -IADL_BASELINE, -SPMSQ_SCORE_ADMISSION) # remove missing variables
  data.to.use = merge(data.to.use, impute[[k]], by='subject_num')
  ## set up data
  #data$stan_date = (as.numeric(data$d_baseline) - 16801)/365 # standardised for regression - too correlated with pre-intervention, so could not include
  data.to.use$age.c = (data.to.use$age -76)/10 # standardised for regression
  data.to.use$CCI_UNADJUSTED_FOR_AGE.c = (data.to.use$CCI_UNADJUSTED_FOR_AGE -2)/2 # standardised for regression
  data.to.use$SPMSQ_SCORE_ADMISSION.c = (data.to.use$SPMSQ_SCORE_ADMISSION-8)/3 # minus median, divide by IQR
#  data.to.use$hosp.num = as.numeric(as.factor(data.to.use$hospital)) # no longer cluster on hospital
#  n.hosp = max(data.to.use$hosp.num)
  data.to.use$ward.num = as.numeric(as.factor(data.to.use$ward))
  n.ward = max(data.to.use$ward.num)
  # censoring (winbugs)
  censored = data.to.use$dc_dest_hosp=='Deceased'
  winbugs.censor = function(){
    data.to.use$TIME_CEN = NA
    data.to.use$TIME_CEN[!censored] = 0 # individuals who fail are given a zero in the censoring time vector
    data.to.use$TIME_CEN[censored] = data.to.use$LOS_TOTAL[censored] - 2.9 # substract 2.9 because all survived until day 3 (min time must be positive)
    data.to.use$LOS = data.to.use$LOS_TOTAL - 2.9 # substract 2.9 because all survived until day 3
    data.to.use$LOS[censored] = NA # individuals who are censored are given a missing value in the vector of failure times ...
  }
  # censoring (jags)
  data.to.use$isCensored = as.numeric(censored)
  data.to.use$LOS = data.to.use$LOS_TOTAL - 2.9 # substract 2.9 because all survived until day 3
  data.to.use$LOS[censored] = NA # individuals who are censored are given a missing value in the vector of failure times ...
  data.to.use$TIME_CEN = NA
  data.to.use$TIME_CEN[!censored] = 0 # individuals who fail are given a zero in the censoring time vector
  data.to.use$TIME_CEN[censored] = data.to.use$LOS_TOTAL[censored] - 2.9 # substract 2.9 because all survived until day 3 (min time must be positive)
  
  # make time interaction
  if(time.interaction != -99){
    data.to.use$intervention.time = data.to.use$INTERVENTION * fp(data.to.use$time.since, power=time.interaction)
    # and for predictions over time
    N.pred = 30
    times = seq(0.01, max(data.to.use$time.since), length.out=N.pred)
    fp.times = fp(times, power=time.interaction)
  }
  # what data to use, depends on primary or secondary outcome
  if(otype == 'primary'){data.to.use = subset(data.to.use, source01 == 'Post-intervention')}
  bdata = data.to.use
  # for bugs:
  n.beta = 7 # number of predictors
  if(otype == 'secondary'){n.beta = 8} # add one predictor (pre or post-intervention)
  if(time.interaction != -99){n.beta = n.beta + 1} # add one predictor for time interaction
  bdata = list(N = nrow(bdata), 
               age=bdata$age.c, 
               LOS=bdata$LOS, 
               TIME_CEN=bdata$TIME_CEN,
               isCensored = bdata$isCensored, # jags only
               ward.num=bdata$ward.num,  n.ward = n.ward, 
               CCI = bdata$CCI_UNADJUSTED_FOR_AGE.c,
               SPMSQ = bdata$SPMSQ_SCORE_ADMISSION.c,
               ADL = as.numeric(bdata$ADL_ADMISSION>0), # change to binary; changed from BASELINE to ADMISSION (August 2018)
               gender=as.numeric(bdata$gender=='Male'), 
               intervention=bdata$INTERVENTION, n.beta=n.beta)
  # add other variables if necessary:
  if(otype == 'secondary'){bdata$pre = as.numeric(data.to.use$source01=='Pre-intervention')} 
  if(time.interaction != -99){
    bdata$intervention.time = data.to.use$intervention.time
    bdata$N.pred = N.pred # export predictions over time too
    bdata$fp.times = fp.times
  } #
  
  ## initial values (do not generate initial values for censored values)
  inits = list(beta=rep(0, n.beta), shape=1, tau.ward=1, zeta=rep(0, n.ward)) # no hospital clustering
  
  # save to lyra
  parms = c('beta', 'median', 'diff', 'zeta.c', 'shape')
  if(time.interaction != -99){parms = c(parms, 'time.pred')}
  outfile = paste('JAGS.Ready.', otype, '.', time.interaction, '.', k,'.RData', sep='')
  setwd('Z:/CHERISH') # move to lyra
  if(time.interaction == -99){save(parms, bdata, inits, MCMC, thin, file=outfile)}
  if(time.interaction != -99){save(times, parms, bdata, inits, MCMC, thin, file=outfile)}
  setwd(this.dir) # move back
  
} # end of imputation loop
