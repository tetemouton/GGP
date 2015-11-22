# SJDM - Changed it to a ggplot as was easier to control and far less code

plot.selectivity <- function(repfile=read.rep("final-rp.glm.rep"), fleetlabs=BET_fleet$fnames, plotcol="#6699CC", Ncols=2)
{
require(reshape2)
require(ggplot2)

theme_set(theme_bw())

    sel <- repfile$SelAtAge                # Matrix of selectivities at age - one row per fishery

    sel <- as.data.frame(t(sel))           # Convert to data.frame and ensure that fisheries are columns not rows
    names(sel) <- fleetlabs              # Give corect fishery labels
    sel$bin <- 1:length(sel[,1])       # Creat an x-axis variable - age-classes

    plot.dat <- melt(sel, id=c("bin")); names(plot.dat)[2:3] <- c("Fishery","Coefficient")   # Format data into the shape required for ggplot e.g. long formate for all fisheries

    # Produce and print plot
    p <- ggplot(plot.dat, aes(x=bin, y=Coefficient)) + geom_density(stat="identity", colour=plotcol, fill=plotcol)
    p <- p + facet_wrap(~ Fishery, ncol=Ncols) #+ theme(strip.text.x = element_text(size=8))
    p <- p + xlab("Age-class (quarters)") + ylab("Selectivity coefficient")
    p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    print(p)   
}
