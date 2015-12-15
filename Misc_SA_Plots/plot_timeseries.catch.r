plot.timeseries.catch = function(catdat = "ALB15/catch.rep", repfile = read.rep("ALB15/plot-12.par.rep"), nocls = NULL,
                                 gear = c("L","L","L","L","L","L","L","L","T","T","T","O","O","O"), region = c(1:8,3,6,8,3,6,8),
                                 all.regions = TRUE, leg.txt.sz = 12, leg.box.sz = 0.4, brwidth = 0.9, legpos = c(0.05, 0.9),
                                 collist = setNames(c("darkslateblue","firebrick3","lawngreen"), c("L","T","O")))
{
  
  require(reshape2)
  require(ggplot2)
  require(dplyr)
  require(magrittr)
  require(grid)
  
  theme_set(theme_bw())
  
  nfsh <- length(gear)
  
  dat <- matrix(scan(catdat, skip=3), ncol = nfsh)
  dat <- as.data.frame(dat)
  
  names(dat) <- 1:nfsh
  
  dat$yrqtr <- repfile$yrs
  
  dat <- melt(dat, measure.vars = 1:(dim(dat)[2]-1), id.vars = "yrqtr", variable.name = "fsh", value.name = "catch")
  dat$fsh <- as.numeric(as.character(dat$fsh))
  dat$gear <- gear[dat$fsh]
  dat$region <- paste("Region", region[dat$fsh])
  dat$year <- floor(dat$yrqtr)
  
  if(all.regions){
    dat %<>% group_by(year, gear) %>% summarise(tcatch = sum(catch))
  } else {
    dat %<>% group_by(year, region, gear) %>% summarise(tcatch = sum(catch))  
  }
  
  
  pl <- ggplot(dat, aes(x = year, y = tcatch/1000, fill = gear)) + geom_bar(stat="identity", width=brwidth) +
    geom_bar(stat="identity", width=brwidth, colour="black", show_guide=FALSE) + scale_fill_manual(name = "gear", values = collist) +# scale_colour_manual(name = "gear", values = collist) +
    xlab('Year') + scale_y_continuous(expand = c(0,0), name = "Catch (1,000's mt)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.text = element_text(size=leg.txt.sz),
          legend.title = element_blank(),
          legend.position = legpos,
          legend.key.size =  unit(leg.box.sz, "cm")) +
    guides(fill = guide_legend(reverse=TRUE))
  
  if(!all.regions) pl <- pl + facet_wrap(~ region, ncol=nocls)
  
  print(pl)
  
}
