plot.T.at.liberty = function(tmp.rep=read.rep(baserep), logscl=FALSE)
{
  
    require(scales)
    require(ggplot2)
      
    at.lib <- tmp.rep$ObsvPredbyLib
 
    if(length(at.lib) == 1) stop("Doesn't look like there's tagging data for this model?")
    
    obslib <- at.lib[, 1]
    prelib <- at.lib[, 2]
    
    theme_set(theme_bw())
    
    pldat <- data.frame(tim=1:length(obslib), obs=obslib, pre=prelib)
    if(logscl) pldat <- data.frame(tim=1:length(obslib), obs=log(obslib + 0.5*min(obslib[obslib > 0])), pre=log(prelib + 0.5*min(obslib[obslib > 0])))
    
    pl <- ggplot(data = pldat, aes(x = tim, y = obs)) + geom_point(colour = alpha("red",0.5), size = 3)
    pl <- pl + geom_line(aes(x = tim, y = pre), colour = alpha("black",0.7), size = 0.9)
    pl <- pl + xlab("Periods at liberty (quarters)") + ylab("No. tag returns")
    if(logscl) pl <- pl + ylim(min(pldat$obs), max(c(pldat$obs, pldat$pre))) + ylab("log(No. tag returns)")
    pl <- pl + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    print(pl)   
    
}
