library(dplyr)
library(reshape2)
library(magrittr)
library(ggplot2)
library(scales)
library(grid)

tmpdir <- "C:/Users/SamM/Documents/VM_folders/tmp_working/model_runs/set2/Inf0.4-Scl0.61-Strt2014"

theme_set(theme_bw())

    setwd(tmpdir)
    load("alb.hcr.summary.storage.RData")
    
    store$ann.SBSB0 <- store$ann.SB/store$meanSB0
    store$qtr.SBSB0 <- store$qtr.SB/store$meanSB0
    
    store$term.SBSB0 <- sapply(1:dim(store$qtr.SBSB0)[2], function(x) mean(store$qtr.SBSB0[(dim(store$qtr.SBSB0)[1]-3):dim(store$qtr.SBSB0)[1],x,1]))
    store$p.above.0.2 <- length(which(store$term.SBSB0 > 0.2))/length(store$term.SBSB0)
    store$p.above.0.45 <- length(which(store$term.SBSB0 > 0.45))/length(store$term.SBSB0)
        
    save(store, file="alb.hcr.summary.storage.full.RData")
    
    y.lim <- 350*1000/store$meanSB0
    modnm <- "Low natural mortality"
    
    windows()
    plot(store$ann.SBSB0[55:84,,], store$ann.ca.LL[55:84,,])    

    hist(store$term.SBSB0, breaks=30, main="", xlab="SB / SB-F=0", col="#6699CC")
    box()
    abline(v=mean(store$term.SBSB0), col="#CC0000", lty=2, lwd=2)
    abline(v=quantile(store$term.SBSB0, 0.5), col="#CC0000", lty=3, lwd=2)
    savePlot(file="Hist-SBSB0.png", type="png")
    
    pldat <- melt(store$qtr.SB)
    pldat$simno <- as.character(pldat$Sims)
    pldat %<>% filter(Yrqtrs >= 1996)

    windows(2000,1200)
        pl <- ggplot(pldat, aes(x=Yrqtrs, y=value/store$meanSB0, fill=simno)) + #geom_rect(data=NULL, aes(xmin=-Inf,xmax=2013.75,ymin=-Inf,ymax=Inf), fill=grey(0.99)) +
                     geom_rect(data=NULL, aes(xmin=2013.75,xmax=Inf,ymin=-Inf,ymax=Inf), fill=grey(0.95)) +
                     geom_line(colour=alpha("black", 0.2), size=0.6, show_guide=FALSE) + xlab("Years") + ylab(bquote("SB/SB"["F"==0])) +
                     geom_vline(xintercept=2013.75, colour=alpha("black", 0.5), size=0.2) +
                     ylim(0, y.lim) + 
                     scale_x_continuous(expand=c(0.015,0.015)) + annotate("text", x=2005, y=y.lim, label=paste0("n = ", length(store$term.SBSB0)), size=6) +
                     annotate("text", x=2020, y=y.lim, label=modnm, size=6) +
                     geom_segment(aes(x=2013.75, y=0.45, xend=2043.75, yend=0.45), colour="#00FF00", size=0.6, linetype="dashed") +
                     geom_segment(aes(x=2013.75, y=0.2, xend=2043.75, yend=0.2), colour="#FF0000", size=0.6, linetype="dashed") +
                     theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())
        pl
    savePlot(file="Projected_trajectories.png", type="png")

    plm <- pl + theme(plot.margin=unit(c(0.25,0.01,0.25,0.25), "cm"))

    windows(2000,1000)
    pltdat <- data.frame(value=store$term.SBSB0*store$meanSB0)
    tmp.txt <- bquote("S"*widetilde(B/S)*"B"["F"==0] == .(round(median(store$term.SBSB0), 2)))

    plt <- ggplot(pltdat, aes(x=value/store$meanSB0)) + geom_histogram(col="black", fill="#6699CC", binwidth=0.02) + coord_flip() + xlim(0, y.lim) +
                  geom_vline(xintercept=0.45, colour=alpha("#00FF00", 0.9), size=0.6, linetype="dashed") +
                  geom_vline(xintercept=0.2, colour=alpha("#FF0000", 0.8), size=0.6, linetype="dashed") +
                  scale_y_discrete(breaks=pretty_breaks(n=3)) + annotate("text", x=0.95*y.lim, y=1, label=deparse(tmp.txt), size=4, parse=TRUE, vjust=0, hjust=0) +
                  theme(panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank(),
                        axis.ticks.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.title.y=element_blank(),
                        panel.background = element_rect(fill=grey(0.95)),
                        plot.margin=unit(c(0.25,0.25,0.25,-0.10), "cm"))
    #plt


    grid.newpage()
    pushViewport(viewport(layout = grid.layout(5,7)))
    
    print(plm, vp = viewport(layout.pos.row=1:5, layout.pos.col=1:6))    
    print(plt, vp = viewport(layout.pos.row=1:5, layout.pos.col=7))

    savePlot(file="Projected_trajectories_wMargHist.png", type="png")




    pdat <- melt(store$qtr.SB.reg)
    pdat %<>% mutate(simno = as.character(Sims),
                     Reg = as.character(regions))

    pdat$Reg <- factor(pdat$Reg, levels=as.character(c(1,4,7,2,5,8,3,6))) # Reorder the regions so that they are arranged in facet_wrap below to match how they appear on a map

    windows(2000,1200)
        plr <- ggplot(pdat, aes(x=Yrqtrs, y=value/1000, fill=simno)) + geom_line(colour=alpha("black", 0.2), size=0.3, show_guide=FALSE) + xlab("Years") + ylab("Adult biomass") +
                     geom_vline(xintercept=2013.75, colour=alpha("red", 0.5), size=0.3) + #ylim(0, 420) + 
                     facet_wrap(~ Reg, scales="free_y") + expand_limits(y=0) +
                     scale_x_continuous(expand=c(0.015,0.015)) + #annotate("text", x=1990, y=410, label=paste0("n = ", length(store$term.SBSB0)), size=8, col="grey") +
                     theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())
        plr
    savePlot(file="Projected_trajectories_ByRegion.png", type="png")




# Catch figures

    cdat <- melt(store$ann.ca.LL)
    cdat %<>% mutate(simno = as.character(Sims)) %>% filter(Years >= 1996)

    windows(2000,1200)
        cl <- ggplot(cdat, aes(x=Years, y=value/1000, fill=simno)) + #geom_rect(data=NULL, aes(xmin=-Inf,xmax=2013.75,ymin=-Inf,ymax=Inf), fill=grey(0.99)) +
                     geom_rect(data=NULL, aes(xmin=2013.75,xmax=Inf,ymin=-Inf,ymax=Inf), fill=grey(0.95)) + ylim(0, 90) +
                     geom_line(colour=alpha("black", 0.2), size=0.6, show_guide=FALSE) + xlab("Years") + ylab("LL Catch (1,000's mt)") +
                     geom_vline(xintercept=2013.75, colour=alpha("black", 0.5), size=0.2) +
                     scale_x_continuous(expand=c(0.015,0.015)) + expand_limits(y=0) +
                     theme(panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())
        clm <- cl + annotate("text", x=2005, y=90, label=paste0("n = ", length(store$term.SBSB0)), size=8, col=grey(0.4))
        clm
    savePlot(file="Projected_catch_trajectories_check.png", type="png")


    tmpca <- store$ann.ca.LL[55:84,,1]
    casum <- tmpca %>% as.data.frame() %>% melt() %>% group_by(variable) %>% 
                       summarise(meanC = mean(value), sdC = sd(value)) %>%
                       mutate(cvC = sdC/meanC)

        cltdat <- data.frame(value=casum$meanC)

        tmpca <- melt(tmpca)
        cltdat <- data.frame(value=tmpca$value)

        clt <- ggplot(cltdat, aes(x=value/1000)) + geom_histogram(col="black", fill="#6699CC", binwidth=1) + coord_flip() + xlim(0, 90) +
                      scale_y_discrete(breaks=pretty_breaks(n=3)) +
                      theme(panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            axis.ticks.y=element_blank(),
                            axis.text.y=element_blank(),
                            axis.title.y=element_blank(),
                            panel.background = element_rect(fill=grey(0.95)),
                            plot.margin=unit(c(0.25,0.25,0.25,-0.10), "cm"))

# Combined adult biomass and catch of LL fisheries

    plm <- pl + theme(plot.margin=unit(c(0.25,0.05,0.25,0.25), "cm"))
    plt <- plt + theme(plot.margin=unit(c(0.25,0.25,0.25,0.05), "cm"))
    clm <- cl + theme(plot.margin=unit(c(0.25,0.05,0.25,0.25), "cm"))

    windows(2400,1600)

        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2,7)))
        
        print(plm, vp = viewport(layout.pos.row=1, layout.pos.col=1:6))
        print(plt, vp = viewport(layout.pos.row=1, layout.pos.col=7))
        print(clm, vp = viewport(layout.pos.row=2, layout.pos.col=1:6))
        print(clt, vp = viewport(layout.pos.row=2, layout.pos.col=7))

    savePlot(file="Projected_trajectories_SBandCA.png", type="png")




#____________________________________________________________________________________________________________
# Plot showing the HCR employed
        

    C2013 <- 77.029
    TRPsclr <- 0.61
    xscl <- seq(0, 0.6, 0.01)
    yscl <- seq(0, 1.2*TRPsclr*C2013, 0.03)
    
    run_HCR = function(status=store$eststatus[i,m,j], RPs=c(0.45,0.2), betas=c(0,0,1), cap=TRUE)
    {        
        if(status >= RPs[1]) scalar.out <- 1        
        if(status < RPs[1] & status >= RPs[2]) scalar.out <- 1 - (status - RPs[1])*((1 - betas[1])/(RPs[2] - RPs[1]))        
        if(status < RPs[2]){
          
            if(cap){              
                scalar.out <- betas[2]              
            } else {                
                scalar.out <- betas[1] - (status - RPs[2])*((1 - betas[1])/(RPs[2] - RPs[1]))*betas[3]                
            }           
        }        
        return(scalar.out)      
    }

    yHCR <- sapply(xscl, function(x) run_HCR(x, RPs=c(0.4,0.2), betas=c(0.05,0.05,1), cap=TRUE))

    windows(1700,1200)
        par(mar=c(5,5,3,3))
        tmp.txt <- bquote("Stock status (SB/SB"["F"==0]*")")

        plot(1, 1, type="n", xlim=range(xscl), ylim=range(yscl), xlab=tmp.txt, ylab="Approx. catch (1,000's mt)", las=1, cex.axis=1.6, cex.lab=1.5)
        abline(v=0.2, lty=2, col="#FF0000", lwd=2.5)
        abline(v=0.45, lty=2, col="#00CC66", lwd=2.5)
        abline(v=0.4, col=grey(0.8), lwd=2.5)
        lines(xscl, yHCR*TRPsclr*C2013, col="#0033CC", lwd=2.5)
        mtext("Target", 3, line=0.1, adj=0.75, cex=1.4, col="#00CC66")
        mtext("Limit", 3, line=0.1, adj=0.33, cex=1.4, col="#FF0000")
    savePlot(file="C:/Users/SamM/Documents/VM_folders/tmp_working/HCRplot.png", type="png")


#____________________________________________________________________________________________________________
# Plot comparing histograms of status and catch over the three assessment models - ref, low M, high M

# First pull in the reference case
    load("C:/Users/SamM/Documents/VM_folders/tmp_working/model_runs/set2/Inf0.4-Scl0.61-Strt2014/alb.hcr.summary.storage.full.RData")
    stdat <- data.frame(status=store$term.SBSB0, model="Reference")

    ctdat <- melt(store$ann.ca.LL)
    ctdat %<>% filter(Years > 2013) %>% mutate(simno = as.character(Sims),
                                               model = "Reference") 

# Then pull in the low M and bind with the above
    load("C:/Users/SamM/Documents/VM_folders/tmp_working/model_runs/set3/Inf0.4-Scl0.59-Strt2014_lowM_run504/alb.hcr.summary.storage.full.RData")
    tmp <- data.frame(status=store$term.SBSB0, model="Low M")
    stdat <- rbind(stdat, tmp)
    
    tmp <- melt(store$ann.ca.LL)
    tmp %<>% filter(Years > 2013) %>% mutate(simno = as.character(Sims),
                                               model = "Low M")
    ctdat <- rbind(ctdat, tmp)

# Finally, pull in the high M and bind with the above
    load("C:/Users/SamM/Documents/VM_folders/tmp_working/model_runs/set3/Inf0.4-Scl0.59-Strt2014_highM_run508/alb.hcr.summary.storage.full.RData")
    tmp <- data.frame(status=store$term.SBSB0, model="High M")
    stdat <- rbind(stdat, tmp)
    
    tmp <- melt(store$ann.ca.LL)
    tmp %<>% filter(Years > 2013) %>% mutate(simno = as.character(Sims),
                                             model = "High M")
    ctdat <- rbind(ctdat, tmp)
    ctdat$model <- factor(ctdat$model, levels=c("Reference","Low M", "High M")) 
     
    windows(2100,1500)
    theme_set(theme_bw())
        tmp.txt <- bquote("Stock status (SB/SB"["F"==0]*")")

        tmpl <- sapply(c("Reference","Low M","High M"), function(x) bquote(tilde(X) == .(round(median(stdat[stdat$model == x, "status"]), 2))))
        tmp.d <- data.frame(x=c(0.75,0.75,0.75), y=c(37,37,37), label=sapply(tmpl, deparse), model=c("Reference","Low M","High M"))
        
        tmpl <- paste(round(sapply(c("Reference","Low M","High M"), function(x) length(which(stdat$status[stdat$model==x] < 0.2))/length(stdat$status[stdat$model==x]))*100), "%")
        tmp.e <- data.frame(x=c(0.05,0.05,0.05), y=c(37,37,37), label=tmpl, model=c("Reference","Low M","High M"))

        stlt <- ggplot(stdat, aes(x=status)) + geom_histogram(col="black", fill="#6699CC", binwidth=0.03) + xlim(0, 1) +
                       geom_vline(xintercept=0.45, colour=alpha("#00FF00", 0.9), size=0.6, linetype="dashed") +
                       geom_vline(xintercept=0.2, colour=alpha("#FF0000", 0.8), size=0.6, linetype="dashed") +
                       facet_wrap(~ model, ncol=1) + ggtitle(label="Status comparison") +
                       xlab(tmp.txt) + ylab("Frequency") +
                       geom_text(data=tmp.d, aes(x=x, y=y, label=label), parse=TRUE) +
                       geom_text(data=tmp.e, aes(x=x, y=y, label=label), colour=alpha("#FF0000", 0.6)) +
                       theme(panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),
                             plot.margin=unit(c(0.25,0.25,0.25,-0.10), "cm"))
        #stlt
        tmpl <- sapply(c("Reference","Low M","High M"), function(x) bquote(tilde(X) == .(round(median(ctdat[ctdat$model == x, "value"])/1000, 1))))
        tmp.d <- data.frame(x=c(10,10,10), y=c(1600,1600,1600), label=sapply(tmpl, deparse), model=c("Reference","Low M","High M"))

        ctlt <- ggplot(ctdat, aes(x=value/1000)) + geom_histogram(col="black", fill="#FF9999", binwidth=0.8) +                      
                       facet_wrap(~ model, ncol=1) + ggtitle(label="LL Catch comparison") +
                       xlab("Annual catch (1,000's mt)") + ylab("Frequency") +
                       geom_text(data=tmp.d, aes(x=x,y=y,label=label), parse=TRUE) +
                       theme(panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),
                             plot.margin=unit(c(0.25,0.25,0.25,-0.10), "cm"))
        #ctlt

# Now calculate delta C 
    dtdat <- ctdat
    keep.yrs <- seq(2014,2041,3)
    dtdat %<>% filter(Years %in% keep.yrs)
    dtdat$delt <- c(NA,dtdat$value[-1]-dtdat$value[-length(dtdat$value)])
    dtdat$rdelt <- c(NA,(dtdat$value[-1]-dtdat$value[-length(dtdat$value)])/dtdat$value[-length(dtdat$value)])*100
    dtdat %<>% filter(!Years == 2014)

        tmpl <- sapply(c("Reference","Low M","High M"), function(x) bquote(sigma == .(round(sd(dtdat[dtdat$model == x, "delt"])))))
        tmp.d <- data.frame(x=c(30000,30000,30000), y=c(600,600,600), label=sapply(tmpl, deparse), model=c("Reference","Low M","High M"))

        dtlt <- ggplot(dtdat, aes(x=delt)) + geom_text(data=tmp.d, aes(x=x,y=y,label=label), parse=TRUE) +
                       facet_wrap(~ model, ncol=1) + ggtitle(label=bquote(Delta~"Catch comparison")) +
                       xlab("Change in catch (mt)") + ylab("Frequency") +
                       geom_histogram(col="black", fill="#66CC99", binwidth=1500) +                      
                       geom_vline(xintercept=0.45, colour=grey(0.5), size=0.6, linetype="dotted") +
                       theme(panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),
                             plot.margin=unit(c(0.25,0.25,0.25,-0.10), "cm"))
        
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2,3)))
        
        print(stlt, vp = viewport(layout.pos.row=1:2, layout.pos.col=1))    
        print(ctlt, vp = viewport(layout.pos.row=1:2, layout.pos.col=2))
        print(dtlt, vp = viewport(layout.pos.row=1:2, layout.pos.col=3))
    
    savePlot(file="C:/Users/SamM/Documents/VM_folders/tmp_working/AssMod_Comparisons_Histogram.png", type="png")
