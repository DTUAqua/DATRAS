dat <- dd[["CA"]]

fitvbg <- function(dd,L0=0){
  ## Lookup time of year by haul.id
  timeOfYear <- dd[["HH"]]$timeOfYear
  names(timeOfYear) <- as.character(dd[["HH"]]$haul.id)
  ## Convert to fractional age
  fracAge <- dd[["CA"]]$Age+timeOfYear[as.character(dd[["CA"]]$haul.id)]
  size <- dd[["CA"]]$LngtCm
  plot(fracAge,size)
  ## Initial Guess: Fit linear model
  slope <- coef(lm(size~fracAge))["fracAge"]
  names(slope) <- NULL
  Linf <- max(size)
  k <- slope/Linf
  start <- c(k=k,Linf=Linf,t0=0.1)
  ##f <- function(t) Linf - (Linf - L0) * exp(-k * (t - t0))
  fit <- nls(size ~ Linf - (Linf - L0) * exp(-k * (fracAge - t0)),
             start=start)
  ans <- as.list(coef(fit))
  ans$L0 <- L0
  ans
}

vbg <- fitvbg(dd)
vbg <- list(Linf=150,L0=1,k=20/150,t0=0.1,L0=0)
plotVBG(dd,scale=2,ylim=c(0,60),col=c(2,4),lwd=2,by=paste(Year,Quarter),model=vbg)


dd <- subset(d,Species=="Gadus morhua")

