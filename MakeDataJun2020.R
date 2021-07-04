# MakeDataJun2020.R
# make data from SPSS file, based on MakeDataAug2018.R
# version without redcap data for dates
# June 2020
library(foreign)
library(reshape2)
library(dplyr)
library(janitor)

## data via email from 1 Aug 2018 (ignore warnings)
data = read.spss('data/CHERISH analysis file for adrian including 30 day health status June 2020.sav', to.data.frame = T) 
data = clean_names(data)
data$subject_num = gsub(' ', '', as.character(data$subject_num))
# convert dates
source('spss.convert.dates.R') # 

# check dates
check = dplyr::select(data, subject_num, d_adm_bl, d_hosp_dc)
dplyr::filter(check, subject_num %in% c('1-1','1-101','1-112'))

## make intervention
# from Alison:
#Intervention wards: 2 (Caboolture 4A); 3 (TPCh Thoracic); 5 (Nambour orthopaedic) and 7 (RBWH 9AN)
#Control wards: 1 (Caboolture 3A); 4 (TPCH Medicine); 6 (Nambour Medicine) and 8 (RBWH 9AS)
iwards = c('Caboolture 4A','RBWH 9AN','Nambour Ortho','TPCH Thoracic')
cwards = c('Caboolture 3A','TPCH Medicine','Nambour Medicine','RBWH 9AS')
data$intervention = 0
index = intersect(which(data$ward %in% iwards) , which(data$source01 == 'Post-intervention') ) # in intervention ward in treatment period (fixed 7 Nov 2018)
data$intervention[index] = 1
# treatment as a factor
data$intervention_factor = factor(data$intervention, levels=0:1, labels=c('Control','Intervention'))

# remove notes
data = dplyr::select(data, -starts_with('NOTES'), -starts_with('BL_')) %>%
  mutate(elective = as.numeric(adm_cat=='Elective')) # not sure about this

# extract hospital
data$hospital = ''
for (j in 1:nrow(data)){
  data$hospital[j] = base::strsplit(as.character(data$ward[j]), split=' ')[[1]][1]
}

# save
save(data, file='data/Analysis.Ready30.RData')
