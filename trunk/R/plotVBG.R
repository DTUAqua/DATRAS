## ---------------------------------------------------------------------------
##' Plot size-spectra with von Bertalanffy growth trajectories.
##'
##' This function draws empirical size distributions of a DATRASraw object
##' in order to follow cohorts using the von Bertalanffy growth model.
##' 
##' @title Cohort plot
##' @param d DATRASraw objects to put together.
##' @param model list containing von Bertalanffy growth parameters.
##' @param by Grouping variable to define the individual size-spectra.
##' @param scale Control height of spectra.
##' @param xlim Time range in years.
##' @param ylim Size range in cm.
##' @param col Color of the histograms. Recycled if shorter than number of histograms.
##' @param yearClasses Growth curves are shown for these year classes.
##' @param mar Controls plot margin.
##' @param oma Controls plot outer margin.
##' @param ylab y-axis label
##' @param ... passed to par()
##' @return NULL
##' @examples
##' \dontshow{
##' file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
##' file2 <- system.file("exchange","Exchange3.zip",package="DATRAS")
##' x <- readExchange(file1)
##' y <- readExchange(file2)
##' z <- c(x,y)
##' d <- subset(z,Species=="Gadus morhua")
##' }
##' d <- addSpectrum(d)
##' plotVBG(d)
## ---------------------------------------------------------------------------
plotVBG <- function(d,
                    model = list(k=0.12,Linf=135,L0=1,t0=0.5),
                    by = paste(Year,Quarter),
                    scale=1,
                    xlim=c(min(d$abstime)-1,max(d$abstime)),
                    ylim=c(0,max(d[["HL"]]$LngtCm,na.rm=TRUE)),
                    col=grey(.9),
                    yearClasses=(floor(xlim[1])-10):ceiling(xlim[2]),
                    mar=c(0,0,0,0),oma=c(5,5,5,5),
                    ylab='Length (cm)',
                    ...
                    ){
  ## DATRAS -> script input
  by <- factor(eval(substitute(by),d[["HH"]]))
  ord <- order(tapply(d$abstime,by,min,na.rm=TRUE)) ## Order levels by time
  by <- factor(by,levels=levels(by)[ord])
  have.spectrum <- "N" %in% names(d[["HH"]])
  if(!have.spectrum)d <- addSpectrum(d)
  tmin <- xlim[1]
  tmax <- xlim[2]
  sg <- colnames(d$N)
  lower <- as.numeric(gsub("\\[(.*),(.*)\\)","\\1",sg))
  upper <- as.numeric(gsub("\\[(.*),(.*)\\)","\\2",sg))
  mids <- .5*(lower+upper)
  time <- tapply(d$abstime,by,mean)
  col <- rep(col,length.out=length(time))
  t <- time
  N <- d$N
  muemp <- sapply(split(1:nrow(N),by),function(i)colMeans(N[i,,drop=FALSE]))
  rtimes <- yearClasses+model$t0
  ## Time labels: Month/Year
  MonthYearLabel <- function(t){
    y <- floor(t)
    m <- findInterval(t-y,seq(0,1,length=13))
    paste(month.abb[m],y)
  }
  timenames <- MonthYearLabel(time)
  ## Base plot
  plot.new()
  par(mar=mar,oma=oma,xaxs='r',usr=c(xlim,ylim),...)  
  axis(1,at=t,labels=timenames,las=2)
  axis(2,las=1)
  mtext(ylab,side=2,line=3)
  box()
  ## Draw von Bertalanffy
  f <- function(t)model$Linf-(model$Linf-model$L0)*exp(-model$k*(t-t0))
  for(t0 in rtimes){
    plot(f,t0,tmax,add=TRUE)
    text <- formatC((floor(t0)-1900)%%100,width=2,flag=0)
    t <- max(par('usr')[1]+0.15,t0)
    text(t-0.08,f(t),text,cex=.8,adj=c(0,-1))
  }
  ## Draw spectra
  mu <- base::t(muemp)
  mids <- mids
  mu <- mu/max(mu) ## max(mu) ~ 1 year if scale=1
  for(i in 1:length(time)){
    g <- approxfun(c(lower,upper[length(upper)]),c(mu[i,],0),method='constant')
    x <- as.vector(rbind(lower,upper))
    y <- as.vector(rbind(mu[i,],mu[i,]))*scale
    x <- c(min(x),x,max(x))
    y <- c(0,y,0)
    polygon(time[i]-y,x,col=col[i])
    t0 <- rtimes[rtimes<time[i]]
    if(length(t0)>0){
      x <- f(time[i])
      y <- g(x)*scale
      x0 <- as.vector(rbind(x,x,NA))
      y0 <- as.vector(rbind(time[i],time[i]-y,NA))
      lines(y0,x0,lty='dotted')
    }
  }
  NULL
}

