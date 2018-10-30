# prepare.jags.model.R
# prepare data for primary/secondary outcome with deaths censored run in JAGS on lyra
# lyra file is run.jags.primary.R which creates results read into Rmd file
# called from cherish.stats.report.Rmd
# Oct 2018

# chain size 
MCMC = 5000
thin = 3

# export all to lyra ready for JAGS
this.dir = getwd()

## prepare subgroups
if(is.null(subgroup)==TRUE){ # no subgroups
  data$group = 1 # dummy group number
  n.groups = 1 
}
if(is.null(subgroup)==FALSE){
  if(subgroup=='age'){
    data$group = 1
    data$group[data$age >= 75] = 2
  }
  if(subgroup=='frailty'){
    data$group = 1
    data$group[data$Frailty_Index >= 0.25 & data$Frailty_Index < 0.40] = 2
    data$group[data$Frailty_Index >= 0.40] = 3
  }
  if(subgroup=='hospital'){
    data$group = as.numeric(as.factor(data$hospital))
  }
  n.groups = max(data$group)
}

## loop through 5 imputes
for (k in 1:5){ #
  # add imputed data
  data.to.use = dplyr::select(data, -IADL_BASELINE, -SPMSQ_SCORE_ADMISSION) # remove missing variables
  data.to.use = merge(data.to.use, impute[[k]], by='subject_num')
  # change LOS depending on outcome
  if(otype %in% c('primary','secondary','interaction')){ # los and destination for the unit
    data.to.use$LOS = data.to.use$LOS_UNIT
    data.to.use$censored.var = data.to.use$dc_dest
  } 
  if(otype == 'secondary.total'){ # total los and destination for the hospital
    data.to.use$LOS = data.to.use$LOS_TOTAL
    data.to.use$censored.var = data.to.use$dc_dest_hosp
  }
  ## set up data
  #data$stan_date = (as.numeric(data$d_baseline) - 16801)/365 # standardised for regression - too correlated with pre-intervention, so could not include
  data.to.use$age.c = (data.to.use$age -76)/10 # standardised for regression
  data.to.use$CCI_UNADJUSTED_FOR_AGE.c = (data.to.use$CCI_UNADJUSTED_FOR_AGE -2)/2 # standardised for regression
  data.to.use$SPMSQ_SCORE_ADMISSION.c = (data.to.use$SPMSQ_SCORE_ADMISSION-8)/(-3) # minus median, divide by IQR; negative IQR to give parameter estimates in same direction
  data.to.use$Frailty_Index.c = (data.to.use$Frailty_Index-0.23)/(0.23) # create Frailty_Index because of interaction (median and IQR are the same)
  data.to.use$hosp.num = as.numeric(as.factor(data.to.use$hospital)) # cluster on hospital
  n.hosp = max(data.to.use$hosp.num)
  # censoring (winbugs) - no longer used
  censored = data.to.use$censored.var == 'Deceased'
  winbugs.censor = function(){
    data.to.use$TIME_CEN = NA
    data.to.use$TIME_CEN[!censored] = 0 # individuals who fail are given a zero in the censoring time vector
    data.to.use$TIME_CEN[censored] = data.to.use$LOS[censored] - 2.9 # substract 2.9 because all survived until day 3 (min time must be positive)
    data.to.use$LOS = data.to.use$LOS - 2.9 # substract 2.9 because all survived until day 3
    data.to.use$LOS[censored] = NA # individuals who are censored are given a missing value in the vector of failure times ...
  }
  # censoring (jags)
  data.to.use$isCensored = as.numeric(censored)
  data.to.use$TIME_CEN = NA
  data.to.use$TIME_CEN[!censored] = 0 # individuals who fail are given a zero in the censoring time vector
  data.to.use$TIME_CEN[censored] = data.to.use$LOS[censored] - 2.9 # substract 2.9 because all survived until day 3 (min time must be positive)
  data.to.use$LOS = data.to.use$LOS - 2.9 # substract 2.9 because all survived until day 3
  data.to.use$LOS[censored] = NA # individuals who are censored are given a missing value in the vector of failure times ...

  # make time interaction
  if(time.interaction != -99){
    data.to.use$intervention.time = data.to.use$INTERVENTION * fp(data.to.use$time.since, power=time.interaction)
    # and for predictions over time:
    N.pred = 30
    times = seq(0.01, max(data.to.use$time.since), length.out=N.pred)
    fp.times = fp(times, power=time.interaction)
  }
  # what data to use (in terms of pre- and post-intervention), depends on primary or secondary outcome
  mtype = otype
  if(otype %in% c('primary','interaction','secondary.total')){
    data.to.use = subset(data.to.use, source01 == 'Post-intervention')
    if(otype=='secondary.total'){mtype = 'primary'} # same model for secondary.total
  }
  bdata = data.to.use
  # for bugs:
  n.beta = 8 # number of predictors
  if(is.null(subgroup) == FALSE){
     if(subgroup=='frailty') {n.beta = n.beta + 1} # add one predictor for frailty index main effect
  }
  if(otype == 'secondary'){n.beta = n.beta + 1} # add one predictor (pre or post-intervention)
  if(time.interaction != -99){n.beta = n.beta + 1} # add one predictor for time interaction
  bdata = list(N = nrow(bdata), 
               age=bdata$age.c, 
               LOS=bdata$LOS, 
               TIME_CEN=bdata$TIME_CEN,
               isCensored = bdata$isCensored, # jags only
               hosp.num=bdata$hosp.num, n.hosp = n.hosp, 
               CCI = bdata$CCI_UNADJUSTED_FOR_AGE.c,
               SPMSQ = bdata$SPMSQ_SCORE_ADMISSION.c,
               ADL = as.numeric(bdata$ADL_ADMISSION>0), # change to binary; changed from BASELINE to ADMISSION (August 2018)
               gender=as.numeric(bdata$gender=='Male'), 
               elective=bdata$elective,
               intervention=bdata$INTERVENTION, n.beta=n.beta)
  # add other variables if necessary:
  if(is.null(subgroup) == FALSE){if(subgroup == 'frailty'){bdata$frailty = data.to.use$Frailty_Index.c}}
  if(otype == 'secondary'){bdata$pre = as.numeric(data.to.use$source01=='Pre-intervention')} 
  if(n.groups > 1){bdata$n.int = n.groups - 1} # number of interactions
  if(time.interaction != -99){
    bdata$intervention.time = data.to.use$intervention.time
    bdata$N.pred = N.pred # export predictions over time too
    bdata$fp.times = fp.times
  } #
  # make subgroup x intervention interaction using a matrix
  if(n.groups > 1){
    bdata$group.int = matrix(data=0, nrow=nrow(data.to.use), ncol=n.groups-1)
    for (q in 1:(n.groups-1)){
      bdata$group.int[, q] = data.to.use$INTERVENTION * (data.to.use$group == q+1)
    }
  }
  
  ## initial values (do not generate initial values for censored values)
  inits = list(beta=rep(0, n.beta), shape=1, tau.hosp=1, zeta=rep(0, n.hosp)) # hospital clustering
  if(n.groups > 1){inits$gamma = rep(0, n.groups - 1)} # interactions with the intervention
  
  # save to lyra
  parms = c('beta', 'median', 'diff', 'zeta.c', 'shape', 'rss')
  if(time.interaction != -99){parms = c(parms, 'time.pred')} # add time predictions
  if(n.groups > 1){parms = c(parms, 'gamma')} # add interaction
  # use mtype here, as data is same for primary and secondary.total:
  if(is.null(subgroup) == TRUE){outfile = paste('JAGS.Ready.', otype, '.', time.interaction, '.', k,'.RData', sep='')} 
  if(is.null(subgroup) == FALSE){outfile = paste('JAGS.Ready.', otype, '.', time.interaction, '.', k, '.', subgroup, '.RData', sep='')} 
  setwd('Z:/CHERISH') # move to lyra
  if(time.interaction == -99){save(parms, bdata, inits, MCMC, thin, file=outfile)}
  if(time.interaction != -99){save(times, parms, bdata, inits, MCMC, thin, file=outfile)}
  setwd(this.dir) # move back
  
} # end of imputation loop
