plot.quarterly.movement.matrix = function(tmprep=read.rep("rep"), tmpfrq=read.frq("frq"), parfile="par")
{

  require(ggplot2)
  require(ggmap)
  require(grid)
  require(scales)

  theme_set(theme_bw())

    regions <- tmprep$nReg
    nage <- tmprep$nAges
    qtrs <- tmpfrq$mpy
    linesget <- regions*nage*qtrs

    pos1 <- grep("# movement matrices",readLines(parfile))
    
    mat <- as.matrix(read.table(parfile, skip=pos1, nrows= linesget))
   
    mat1 <- mat[1:regions,]; diag(mat1) <- 0
    mat2 <- mat[(1+regions*nage):(regions+regions*nage),]; diag(mat2) <- 0
    mat3 <- mat[(1+regions*nage*2):(regions+regions*nage*2),]; diag(mat3) <- 0
    mat4 <- mat[(1+regions*nage*3):(regions+regions*nage*3),]; diag(mat4) <- 0

    dim(mat1) <- c(regions*regions,1); dim(mat2) <- c(regions*regions,1); dim(mat3) <- c(regions*regions,1); dim(mat4) <- c(regions*regions,1)

    mat1 <- data.frame(Rate = mat1, mto = as.factor(rep(1:regions,regions)), mfm = as.factor(rep(1:regions, each = regions)))
    mat2 <- data.frame(Rate = mat2, mto = as.factor(rep(1:regions,regions)), mfm = as.factor(rep(1:regions, each = regions)))
    mat3 <- data.frame(Rate = mat3, mto = as.factor(rep(1:regions,regions)), mfm = as.factor(rep(1:regions, each = regions)))
    mat4 <- data.frame(Rate = mat4, mto = as.factor(rep(1:regions,regions)), mfm = as.factor(rep(1:regions, each = regions)))

    maxmov <- max(mat1$Rate, mat2$Rate, mat3$Rate, mat4$Rate)

    pl1 <- ggplot(mat1, aes(x = mfm, y = mto)) + geom_tile(aes(fill = Rate), colour = "black") + scale_fill_gradient(low = "snow", high = "#CC0033", limits = c(0, 1.1*maxmov)) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                  xlab("Source region") + ylab("Receiving region") + ggtitle("Quarter 1")
    
    pl2 <- ggplot(mat2, aes(x = mfm, y = mto)) + geom_tile(aes(fill = Rate), colour = "black") + scale_fill_gradient(low = "snow", high = "#CC0033", limits = c(0, 1.1*maxmov)) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                  xlab("Source region") + ylab("Receiving region") + ggtitle("Quarter 2")
    
    pl3 <- ggplot(mat3, aes(x = mfm, y = mto)) + geom_tile(aes(fill = Rate), colour = "black") + scale_fill_gradient(low = "snow", high = "#CC0033", limits = c(0, 1.1*maxmov)) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                  xlab("Source region") + ylab("Receiving region") + ggtitle("Quarter 3")
    
    pl4 <- ggplot(mat4, aes(x = mfm, y = mto)) + geom_tile(aes(fill = Rate), colour = "black") + scale_fill_gradient(low = "snow", high = "#CC0033", limits = c(0, 1.1*maxmov)) +
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                  xlab("Source region") + ylab("Receiving region") + ggtitle("Quarter 4")

    pushViewport(viewport(layout = grid.layout(2,2)))
    print(pl1, vp = viewport(layout.pos.row=1, layout.pos.col=1))
    print(pl2, vp = viewport(layout.pos.row=1, layout.pos.col=2))
    print(pl3, vp = viewport(layout.pos.row=2, layout.pos.col=1))
    print(pl4, vp = viewport(layout.pos.row=2, layout.pos.col=2))

}
