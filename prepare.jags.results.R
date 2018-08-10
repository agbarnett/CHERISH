# prepare.jags.results.R
# prepare results for primary/secondary outcome with deaths censored run in JAGS on lyra
# combine results from multiple imputation
# called from cherish.stats.report.Rmd
# December 2017

# export all to lyra ready for JAGS
this.dir = getwd()

## loop through 10 imputes
betas = vars = NULL # means and variances for imputed results
pvals = NULL # p-values for hazard ratio
res = NULL # store residual sum of squares
setwd('Z:/CHERISH')
for (k in 1:5){ # just five imputations
  
  # get JAGS results
  # DIC does not work for this model, see https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/b7859e29/
  infile =  paste('JAGS.results.', otype, '.', time.interaction, '.', k,'.RData', sep='')
  load(infile)
  
  # store RSS
  this.rss = subset(rss, chain== 99, select=c(-node, -pvalue, -chain))
  this.rss$k = k
  res = rbind(res, this.rss)
  
  # store pvalues
  this.pval = subset(diff, chain==99, select=c('node','row','pvalue'))
  pvals = rbind(pvals, this.pval)
  
  ## extract predictions over time
  # TO DO
  #time.pred = 
  
  ## extract mean and variance for multiple imputation
  matrix = dplyr::select(chain, -iter, -chain)
  this.beta = colMeans(matrix)
  this.var = cov(matrix)
  betas[[k]] = this.beta
  vars[[k]] = this.var
  
} # end of imputation loop
setwd(this.dir)

# combine
mi.res = summary(MIcombine(betas, vars))

# plot chains (just for last imputation)
to.plot = melt(chain, id.vars=c('iter','chain'))
names(to.plot) = c('iter','chain','variable','value')
chain.plot = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
  geom_line()+
  scale_color_manual(name=NULL, values=2:3)+
  facet_wrap(~variable, scales='free_y')+
  theme_bw()+
  theme(legend.position = 'none')
