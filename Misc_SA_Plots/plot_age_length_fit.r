plot.age.length.fit <- function(alfile="path", frqfile=read.frq("path"), inifile=read.ini("path"), plotfile=read.rep("path"),
                                parfile=read.par("path"), fixlog=FALSE, fix_pars=NA, sdmult=1, ylims=c(30,130), xlbl="Age (quarters)")
{

require(reshape2)  
require(scales)
require(grid)

theme_set(theme_bw())
  
  a <- readLines(alfile)
  nsamp <- as.numeric(a[2])
  pos <- grep("# Year   Month   Fishery   Species", a)
  
  if(length(pos) != nsamp) stop("ERROR: no. samples does not match matrix observations in age_length file")
  
  lfint <- frqfile$dl$lfint   # no. of length intervals
  lffirst <- frqfile$dl$lffirst   # no. of length intervals
  lfwidth <- frqfile$dl$lfwidth   # no. of length intervals

  nage <- inifile$nages
# - length intervals
  lenint <- seq(from=lffirst, to= (lffirst + lfint -1), by=1)
# - age intervals
  ageint <- c(1:inifile$nages)

  alsamps <- list()
  snames <- list(Length=as.character(c(1:nsamp)),Fishdets=c("Year","Month","Fishery",
                 "Species","nobs"))
  samp_tbl <- matrix(0,nrow=nsamp,ncol=5,dimnames=snames)

  for(k in 1:nsamp){
    samp_tbl[k,c(1:4)] <- as.numeric(unlist(strsplit(a[(pos[k]+1)],split="[[:blank:]]+")))
    anames <- list(Length=as.character(lenint),Age=as.character(ageint))
    al_tbl <- matrix(0,nrow=length(lenint),ncol=length(ageint),dimnames=anames)
    
    for(i in 1:lfint){
      al_tbl[i,] <- as.numeric(unlist(strsplit(a[(pos[k]+1+i)],split="[[:blank:]]+")))
      
    }
    samp_tbl[k,5] <- sum(al_tbl)
    alsamps[[k]] <- al_tbl
    
  }

  anames <- list(Length=as.character(lenint),Age=as.character(ageint))
  tot_al_tbl <- matrix(0,nrow=length(lenint),ncol=length(ageint),dimnames=anames)

  for(k in 1:nsamp){
    tot_al_tbl[,] <- tot_al_tbl[,] + alsamps[[k]][,]
    
  }
#  sum(tot_al_tbl)

# Diagnostic plot of fit to age-length data
# Plot age-length data
  if(fixlog){
    Lmin <- fix_pars[1]
    Lmax <- fix_pars[2]
    K <- fix_pars[3]
    
  } else {
    Lmin <- parfile$Lmin
    Lmax <- parfile$Lmax
    K <- parfile$K
    
  }

#   -- plot  --
  obsdat <- melt(tot_al_tbl)

  predat <- data.frame(age = ageint,
                       muL = plotfile$mean.LatAge,
                       sdL = plotfile$sd.LatAge,
                       LL = plotfile$mean.LatAge - sdmult*plotfile$sd.LatAge,
                       UL = plotfile$mean.LatAge + sdmult*plotfile$sd.LatAge)

  print(paste("Upper and lower bounds displayed are", sdmult, "times the SD of length of age"))

  pldat <- obsdat[obsdat$value > 0,]

  xlims <- range(predat$age)

  pl <- ggplot(data=pldat, aes(x=Age, y=Length)) + geom_point(aes(size=value), colour=alpha("#6699CC", 0.7)) +
               geom_line(data=predat, aes(x=age, y=muL), colour=alpha("black",0.6), size=0.8) +
               geom_line(data=predat, aes(x=age, y=LL), colour=alpha(grey(0.5),0.6), size=0.6) +
               geom_line(data=predat, aes(x=age, y=UL), colour=alpha(grey(0.5),0.6), size=0.6) +
               xlab(xlbl) + ylab("Length (cm)") + xlim(xlims) + ylim(ylims) +
               theme(panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     legend.key=element_blank(),
                     legend.title=element_blank(),
                     legend.text = element_text(size=14),
                     legend.position=c(0.9, 0.2),
                     legend.key.size =  unit(0.7, "cm"))

  print(pl)
}
