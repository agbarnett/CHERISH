# prepare.jags.results.R
# prepare results for primary/secondary outcome with deaths censored run in JAGS on lyra
# combine results from multiple imputation
# called from cherish.stats.report.Rmd
# Oct 2018

# get ready to move to lyra
this.dir = getwd()

## prepare subgroup loops
if(is.null(subgroup)==TRUE){ # no subgroups
  n.groups = 1
  store.gamma = FALSE
} 
if(is.null(subgroup)==FALSE){
  if(subgroup=='age'){n.groups = 2}
  if(subgroup=='frailty'){n.groups = 3}
  if(subgroup=='hospital'){n.groups = 4}
  store.gamma = TRUE
  gammas = NULL
}

## loop through 5 imputed data sets
pvals = NULL # p-values for hazard ratio
res = NULL # store residual sum of squares
mi.res = NULL # store parameter estimates
diffs = NULL # all differences
setwd('Z:/CHERISH')

betas = vars = NULL # means and variances for imputed results
for (k in 1:5){ # just five imputations
  
  # get JAGS results
  # DIC does not work for this model, see https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/b7859e29/
  if(is.null(subgroup) == TRUE){infile = paste('JAGS.results.', otype, '.', time.interaction, '.', k,'.RData', sep='')} 
  if(is.null(subgroup) == FALSE){infile = paste('JAGS.results.interaction.-99.', k, '.', subgroup, '.RData', sep='')} 
  load(infile)
  
  # store RSS
  this.rss = subset(rss, chain== 99, select=c(-node, -pvalue, -chain))
  this.rss$k = k
  res = rbind(res, this.rss)
  
  # store pvalues
  this.pval = subset(diff, chain==99, select=c('node','row','pvalue'))
  this.pval$k = k
  pvals = rbind(pvals, this.pval)

  # store gamma (interactions)
  if(store.gamma==TRUE){
    this.gamma = subset(gamma, chain==99, select=c('node','row','mean','lower','upper','pvalue'))
    this.gamma$k = k
    gammas = rbind(gammas, this.gamma)
  }
  
  # add subgroup to diff
  diff$k = k
  diffs = rbind(diffs, diff)

  ## extract mean and variance for multiple imputation
  matrix = dplyr::select(chain, -iter, -chain) # both chains
  this.beta = colMeans(matrix)
  this.var = cov(matrix)
  betas[[k]] = this.beta
  vars[[k]] = this.var
  
} # end of imputation loop

# combine
m.res = summary(MIcombine(betas, vars))

# make nicer variables for use in tables
nicer = subset(m.res, select=c('results','(lower','upper)'))
names(nicer) = c('Mean','Lower','Upper')
nicer$var = paste('beta', 1:nrow(nicer), sep='')

setwd(this.dir) # move back

# plot chains (just for last imputation and just for primary outcome)
if(otype=='primary' & time.interaction==-99 & is.null(subgroup)==TRUE){
  to.plot = melt(chain, id.vars=c('iter','chain'))
  names(to.plot) = c('iter','chain','variable','value')
  chain.plot = ggplot(data=to.plot, aes(x=iter, y=value, col=factor(chain)))+
    geom_line()+
    xlab('Iteration')+
    scale_color_manual(name=NULL, values=2:3)+
    facet_wrap(~variable, scales='free_y')+
    theme_bw()+
    theme(legend.position = 'none')
}

# rename to match old code for median differences
diff = diffs
