# plot.cuminc.R
# function to plot cumulative incidence, called from cherish.stats.report.Rmd
# october 2017

to.plot = NULL
for (k in 1:(length(cuminc)-1)){
  frame = data.frame(cuminc[[k]])
  frame$from = 'Admission'
  frame$to = names(cuminc)[k]
  to.plot = rbind(to.plot, frame)
}
# panel
index = grep(' 2', to.plot$to)
to.plot$panel = 'Discharge'
to.plot$panel[index] = 'Died in hospital'
# group
if(ptype=='two groups'){ # two groups for primary outcome
  index = grep('1 ', to.plot$to)
  to.plot$group = 'Control'
  to.plot$group[index] = 'Intervention'
  colours = 1:2 # colours for lines in plot
}
if(ptype=='four groups'){ # two groups for primary outcome
  to.plot$group = gsub(' 1| 2|', '', to.plot$to)
  colours = 1:4 # colours for lines in plot
}

# add labels 
l1 = data.frame(group=factor('Control'), x=c(30,0), y=c(0,0.83), text=c('Fewer dischages','More discharges'), panel='Discharge', h=c(1,0))
l2 = data.frame(group=factor('Control'), x=c(30,0), y=c(0,0.04), text=c('Fewer deaths','More deaths'), panel='Died in hospital', h=c(1,0))
labels = rbind(l1, l2)

# plot
gplot = ggplot(data=subset(to.plot, time<=30), aes(x=time, y=est, col=factor(group)))+
  geom_line(size=1.2)+
  facet_wrap(~from, scales='free_y')+
  scale_color_manual(name=NULL, values=colours)+
  theme_bw()+
  xlab('Time since admission (days)')+
  ylab('Cumulative proportion')+
  theme(legend.position=c(0.85, 0.5), text=element_text(size=15), panel.grid.minor = element_blank())+
  facet_wrap(~panel, scales='free_y')+
  geom_text(data=labels, aes(x=x, y=y, label=text, hjust=h), col='black')
