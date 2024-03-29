# impute.mice.R
# imute missing baseline data using chained regression
# see vignette https://gerkovink.github.io/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html
# called from cherish.stats.report.Rmd
# Feb 2021
library(mice)

n.impute = 20 # number of imputed data sets; upped to 20 (Oct 2018)

# run or not, used to save time
if(impute.run==FALSE){
  infile = paste('MICE.Results.impute.RData', sep='')
  load(infile)  
}

# run
if(impute.run==TRUE){
  
  # select data with subject number; note, using all data include pre-intervention period
  subject_num = data$subject_num
  cont.vars = make_clean_names(c('age','SPMSQ_SCORE_ADMISSION','IADL_BASELINE','ADL_BASELINE','CCI_UNADJUSTED_FOR_AGE'))
  for.cor = subset(data, select=cont.vars)
  
  # use MICE
  imputed = mice(for.cor, m=n.impute, seed=123) # set seed for consistent result
  imputed
  
  # add back subject number
  impute = list()
  vars = make_clean_names(c('IADL_BASELINE','SPMSQ_SCORE_ADMISSION'))
  for (k in 1:n.impute){
    impute[[k]] = subset(complete(imputed, k), select=vars) # just keep two missing variables
    impute[[k]]$subject_num = subject_num
  }
  
  # plot to compare complete case and imputed
  imp = melt(complete(imputed, 2)) # randomly choose second dataset
  index = melt(is.na(for.cor))
  names(index) = c('row','var','missing')
  to.plot = cbind(imp, index)
  to.plot = subset(to.plot, variable %in% make_clean_names(c('SPMSQ_SCORE_ADMISSION'))) # just SPMSQ (August 2018)
  to.plot$value = round(to.plot$value) # round for plot
  iplot = ggplot(data=to.plot, aes(x=value, fill=factor(missing)))+
    geom_bar()+
    facet_wrap(~variable, scales='free_x')+
    theme_bw()+
    xlab('')+
    ylab('Frequency')+
    scale_fill_manual('Imputed', values=c('goldenrod1','dark red'), labels=c('No','Yes'))+
    theme(legend.position=c(0.2,0.85))
  jpeg('figures/imputed.jpg', width=4.5, height=4.5, units='in', res=300)
  print(iplot)
  dev.off()
  
  # save results
  outfile = paste('MICE.Results.impute.RData', sep='')
  save(iplot, impute, file=outfile)
} # end of if
