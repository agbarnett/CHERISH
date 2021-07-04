# spss.convert.dates.R
# dates are not converting well from SPSS so read from R
  o.date = '1582-10-14'
#  data$d_baseline = as.Date(data$d_baseline/(24*60*60), origin=o.date)
  data$d_adm_bl = as.Date(data$d_adm_bl/(24*60*60), origin=o.date)
#  data$d_d5 = as.Date(data$d_d5/(24*60*60), origin=o.date)
#  data$d_discharge_visit = as.Date(data$d_discharge_visit/(24*60*60), origin=o.date)
  data$d_dc_index_team = as.Date(data$d_dc_index_team/(24*60*60), origin=o.date) # date of discharge from index team
  data$d_hosp_dc = as.Date(data$d_hosp_dc/(24*60*60), origin=o.date) # What was the overall discharge date from hospital?
  data$d_unpl_adm1_m6 = as.Date(data$d_unpl_adm1_m6/(24*60*60), origin=o.date) # unplanned admission date
  data$death_date = as.Date(data$death_date/(24*60*60), origin=o.date) # death date
  data$d_adm1_m6 = as.Date(data$d_adm1_m6/(24*60*60), origin=o.date) # admission within 6 months date
  
  
# check dates
  #
  f = filter(data, 
             !is.na(death_date), 
             !is.na(d_hosp_dc),
             death_date < d_hosp_dc)
  if(nrow(f)>0){cat('Death before discharge')}
  #
  f = filter(data, 
             !is.na(d_unpl_adm1_m6), 
             !is.na(d_dc_index_team),
             d_unpl_adm1_m6 < d_dc_index_team)
  if(nrow(f)>0){
    cat('Six month admission before index discharge\n')
    select(f, subject_num, hospital, d_dc_index_team, d_hosp_dc, d_unpl_adm1_m6)
  }
  #
  f = filter(data, 
             !is.na(d_adm1_m6), 
             !is.na(d_dc_index_team),
             d_adm1_m6 < d_dc_index_team)
  if(nrow(f)>0){
    cat('Six month admission before index discharge\n')
    select(f, subject_num, hospital, d_dc_index_team, d_hosp_dc, d_adm1_m6)
  }
  #
  f = filter(data, 
             !is.na(death_date), 
             !is.na(d_unpl_adm1_m6),
             death_date < d_unpl_adm1_m6)
  if(nrow(f)>0){cat('Six month admission after death')}
  
my_old_dates = function(){
  
## censor dates
  # censor those still alive at 6 months
  censor_days = 183 # 6 months in days
  data = mutate(data, 
                # those with no death are censored at six months
                days_from_unit_dc_to_death = ifelse(death_within_6mths==0, censor_days, days_from_unit_dc_to_death),
                # those with a death beyond six months are censored and the date is blanked
                index = days_from_unit_dc_to_death > censor_days,
                death_within_6mths = ifelse(index==TRUE, 0, death_within_6mths), # no death and ...
                days_from_unit_dc_to_death = ifelse(index==TRUE, censor_days, days_from_unit_dc_to_death)) %>% # ... alive at six months
    select(-index)

  ## make all-cause re-admission time 
  # there are no dates beyond six months
  data = mutate(data, 
                # make readmission time
                d_adm1_m6 = ifelse(readmit_6mth==0, d_dc_index_team + censor_days, d_adm1_m6), # set date to six months from index discharge for those with no readmission
                d_adm1_m6 = as.Date(d_adm1_m6, origin = '1970-01-01'),
                readmission_time = as.numeric(d_adm1_m6 - d_dc_index_team))
  
  
  #
  #sample_n(datax, 5) %>%
  #  select(d_dc_index_team, d_adm1_m6, readmission_time, readmit_6mth)
  
  # checks
  filter(data, is.na(days_from_unit_dc_to_death)) %>%
    select(subject_num, death_within_6mths,  days_from_unit_dc_to_death, death_date, d_hosp_dc, d_dc_index_team)
  
}
