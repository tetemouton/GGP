plot.overall.composition.fit = function(filename="P:/bet/2014/assessment/RefCase/weight.fit", xlabel="Weight (kg)", remove.fsh="TRUE",
                                   VecFsh=spp_fleets$fnames, Ncols=4, line.wdth=1.2, lincol="#FF3333", fillcol="#6699CC")
{

    require(ggplot2)
    require(reshape2)
    
    theme_set(theme_bw())
    
    Nfsh <- scan(filename, nlines=1, skip=2) - 1   # Determine the number of fisheries from file header
    Nskips <- scan(filename, nlines=1, skip=4)   # Determine the number of lines in the matrix for each fishery, from file header
    size.pars <- scan(filename, nlines=1, skip=1)  # Extract the parameters that determine the size bins - no. bins, first bin size, bin width
    sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])   # Construct the size bins from the file header
    
    VecFsh <- 1:(Nfsh)   # Vector of fisheries numbers - just numeric for now
    LineKeep <- (VecFsh-1) * (Nskips + 6) + 1   # Identify the lines of the observed size frequencies for the fisheries
    
    dat <- readLines(filename)   # Read in the file as text - run time could be reduced by only reading in from '# fishery totals' down but no skiplines argument in readLines - will have a hunt
    dat <- dat[(grep("totals",dat)+4):length(dat)]   # Remove all unwanted data above the fishery totals

    dat.obs <- dat[LineKeep]   # This is the only observed data we want keep - pulls out vector for the fishery then skips down to the next fishery and grabs vector, etc. etc.
    dat.obs <- as.data.frame(t(read.table(text=dat.obs, nrows=length(LineKeep))))   # Get it in the right format and transpose
    names(dat.obs) <- VecFsh   # Match the fishery names to the columns
    
    keep.fsh = c(na.omit(ifelse(apply(dat.obs,2,sum) > 0, names(dat.obs), NA)), "sizebin", "set")   # Used to identify which fisheries have data - if all zeros then removed later on if remove.fsh == "TRUE"
    
    dat.obs$sizebin <- sizebins   # Add sizebins - becomes the x axis later on
    dat.obs$set <- "Observed"   # Neet to identify this data as observed


# Same process as above done for predicted sizes - could have done them simultaneously but harder to get them in the right format for plotting
    dat.pred <- dat[LineKeep+1]
    dat.pred <- as.data.frame(t(read.table(text=dat.pred, nrows=length(LineKeep))))
    names(dat.pred) <- VecFsh
    dat.pred$sizebin <- sizebins
    dat.pred$set <- "Predicted"

# Combine observed and predicted datasets
    dat.full <- rbind(dat.obs, dat.pred)

    if(remove.fsh == "TRUE")    dat.full <- dat.full[,match(keep.fsh,names(dat.full))]   # If true only fisheries with data will be plotted, if false then will be plotted as zeros

    plot.dat <- melt(dat.full, id=c("set","sizebin")); names(plot.dat)[3:4] <- c("Fishery","freq")   # Format data into the shape required for ggplot

# Produce and print plot
    p <- ggplot(plot.dat[plot.dat$set == "Observed",], aes(x=sizebin, y=freq)) + geom_bar(stat="identity", colour=fillcol, fill=fillcol)
        p <- p + facet_wrap(~ Fishery, ncol=Ncols, scales="free_y")
        p <- p + xlab(xlabel) + ylab("Samples")
        p <- p + geom_line(data=plot.dat[plot.dat$set == "Predicted",], aes(x=sizebin, y=freq), colour=lincol, size=line.wdth)
        p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    print(p)
    
}

















