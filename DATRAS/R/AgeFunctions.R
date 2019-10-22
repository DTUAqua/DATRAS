
##' Calculate conditional expectation of some function of length given age.
##'
##' Find E(f(L)|A) in terms of p(L), p(A|L) and f(L), using Bayes formula.
##' @title Calculate conditional expectation of some function of length given age.
##' @param pal matrix with age-length key, i.e. p(A|L). Columns correspond to age groups.
##' @param pl vector with length distribution, i.e. p(L). 
##' @param fl vector of function values at length, i.e. f(L).
##' @return vector of expectations
bayes <- function(pal,pl,fl){
  ## find length distr. given age p( l | a)
  ## p( l_i | a ) = p( a | l_i ) * p(l_i) / sum( p( a | l) p(l) )
  pla = pal;
  for(i in 1:ncol(pla)){
    pa = sum(pal[,i]*pl,na.rm=TRUE)
    pla[,i] = pal[,i]*pl / pa; 
  }
  ## find E(f(l) | a ) = sum( f(l)*p(l|a) )
  Ewa = numeric(ncol(pla))
  for(a in 1:ncol(pla)){
    Ewa[a] = sum( fl*pla[,a],na.rm=TRUE)
  }
  names(Ewa) <- colnames(pal)
  Ewa
}

##' Calculate empirical (raw) age length key. 
##'
##' Ages < minAge are excluded. Ages > maxAge are pooled into a plus group.
##' @title Calculate empirical (raw) age length key. 
##' @param d DATRASraw object
##' @param minAge Minimum age
##' @param maxAge Maximum age (plus group)
##' @return matrix with ALK
rawALK <- function(d,minAge,maxAge){
  checkSpectrum(d);
  if(any(xtabs(Age==minAge~Year,data=d[[1]])==0)) stop(paste("Some years have no observations of age",minAge));
  d[[1]] = subset(d[[1]],Age>=minAge);
  d[[1]]$Age[ d[[1]]$Age > maxAge ] = maxAge;
  cm.b=attr(d,"cm.breaks")
  pl = colSums(d$N)/sum(d$N)
  d[[1]]$sizeGroup <- cut(d[[1]]$LngtCm, breaks = cm.b, 
                          right = FALSE)
  tab = xtabs(NoAtALK ~ sizeGroup + Age, data = d[[1]])
  pal = tab/rowSums(tab)
  colnames(pal)[ncol(pal)] <- paste(colnames(pal)[ncol(pal)],"+",sep="")
  pal
}

##' Calculate weight by age group in the survey accounting for length-stratified subsampling of age.
##'
##' Empirical application of Bayes formula.
##' @title Calculate weight by age group in the survey accounting for length-stratified subsampling of age.
##' @param d DATRASraw object
##' @param minAge minimum age group to consider
##' @param maxAge maximum age group (plus group)
##' @param alk.type method to calculate age-length key
##' @return A vector with weight-at-age
weightAtAge<-function(d,minAge,maxAge,alk.type=c("raw","smooth")){
  checkSpectrum(d);
  alk.type <- match.arg(alk.type)
  pl = colSums(d$N)/sum(d$N)
  if(alk.type=="raw"){
    pal <- rawALK(d,minAge,maxAge)
  }
  else if(alk.type=="smooth"){
    ALKm <- fitALK(d,minAge,maxAge)
    pal <- predict(ALKm,newdata=d,minAge,maxAge,type="ALK")[[1]]
    ## TODO: Add colnames to pal (in predict.ALKm)
  }

  ## fit W = a*L^b
  cm.b=attr(d,"cm.breaks")
  WAAmodel=lm(log(IndWgt)~I(log(LngtCm)),data=d[[1]])
  WL = exp(predict(WAAmodel,newdata=data.frame(LngtCm=cm.b[-length(cm.b)],IndWgt=NA)))

  bayes(pal,pl,WL)  
}


##' Plot bubbles on a map with area proportional to "response".
##'
##' See also addWeightByHaul()
##' @title Plot bubbles on a map with area proportional to "response".
##' @param d DATRASraw object
##' @param response name of variable to plot
##' @param scale scale size of bubbles
##' @param col.zero color for zero hauls
##' @param pch.zero pch for zero hauls
##' @param rim Add blue rim to bubbles? (defaults to FALSE)
##' @param ... extra arguments to plot
##' @return nothing
bubblePlot <- function(d,response="HaulWgt",scale=NULL,col.zero="red",pch.zero="+",rim=FALSE,...){
  d[[2]]$resp.var <- d[[2]][[response]]
  if(is.null(scale)) scale = 4/max(sqrt(d[[2]]$resp.var),na.rm=TRUE)
  plot(d$lon,d$lat,type="n",xlab="Longitude",ylab="Latitude")
  map('worldHires',fill=TRUE,plot=TRUE,add=TRUE,col=grey(0.5))
  points(d$lon,d$lat,pch=16,cex=scale*sqrt(d[[2]]$resp.var),...)
  if(rim) points(d$lon,d$lat,cex=scale*sqrt(d[[2]]$resp.var),pch=1,lwd=0.3,col="blue")
  zero=subset(d,resp.var==0)
  points(zero$lon,zero$lat,pch=pch.zero,col=col.zero)
}


##' Calculate total biomass by haul and add to HH-records
##'
##' Fits W = a*L^b and applies to all length data (standardized to one minute).
##' Thus, this only makes sense for one species.
##' @title Calculate total biomass by haul and add to HH-records 
##' @param d DATRASraw object
##' @param to1min divide by haul duration in minutes? (defaults to TRUE)
##' @return DATRASraw object
addWeightByHaul <-function(d, to1min=TRUE){
  checkSpectrum(d);
  m=lm( log(IndWgt) ~ log(LngtCm),data=subset(d[[1]],IndWgt>0))
  cm.breaks = attr(d,"cm.breaks")[-1]-0.5
  tmp=d[[1]][ 1:length(cm.breaks) ,]
  tmp$LngtCm = cm.breaks
  tmp$Wgt = exp( predict(m,newdata=tmp) )
  LW=tmp$Wgt  
  WgtByHaul<-function(i){
    d[[2]]$N[i,] %*% LW
  }
  d[[2]]$HaulWgt=unlist(lapply(1:nrow(d[[2]]),WgtByHaul))
  if(to1min) d[[2]]$HaulWgt = d[[2]]$HaulWgt/d[[2]]$HaulDur
  d
}

