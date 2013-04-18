## @title Find all variable names in formula which is also in hydro data
## @param formula A formula object
## @param x a DATRASraw object
## @return a vector of variable names
xtraVars <- function(formula,x){
  intersect(all.vars(formula),names(x[[2]]))
}

##' @title Fit part of the continuation ratio model using GAMs (helper function)
##' @param a Condition on being at least this age.
##' @param ages Vector with consecutive ages to consider, last age is plus group. 
##' @param AL Age-length data from a DATRASraw object
##' @param model Model formula (string or formula), or a vector of strings specifying the formula for each age group.
##' @param gamma Multiplier for AIC score (see ?gam)
##' @param autoChooseK Automatic choice of the max. dimension for the basis used to represent the smooth term for spatial ALK. See ?s in the mgcv-package.
##' @param useBIC Use Bayesian Information Criterion for smoothness selection instead of AIC.
##' @param varCof Use varying coefficients model for spatial effect.
##' @param maxK Maximum k to use. Only applies if autoChooseK is TRUE. 
##' @param verbose Print model summary?
##' @param ... Extra parameters to gam()
##' @return Object of class '"gam"'
fitALKone<-function(a,ages,AL,model,gamma,autoChooseK=FALSE,useBIC=FALSE,varCof=FALSE,maxK=100,verbose=FALSE,...){
  if(length(model)>1){
    idx=which(ages==a);
    f <- as.formula(model[idx])
  } else {
    f <- as.formula(model)
  }
  require(mgcv,quietly=TRUE)
  myd=subset(AL,Age>=a);
  myd$cra=as.factor(myd$Age>a);

  ## automatic choice of k - overrides model arguments
  if(autoChooseK){
    uniqueCovs = length( unique( paste( myd$lon, myd$lat)));
    if(!varCof){
      k = min(maxK, uniqueCovs-1);
      f = as.formula(paste("cra~LngtCm+s(lon,lat,k=",k,",bs='ts')"));
      if(uniqueCovs<10) f=as.formula("cra~LngtCm");
    } else {
        k = min(maxK, uniqueCovs/2-1);
        f = as.formula(paste("cra~s(lon,lat,by=LngtCm,k=",k,",bs='ts')+s(lon,lat,k=",k,",bs='ts')"));
        if(uniqueCovs<10) f=as.formula("cra~LngtCm");
      }
    if(useBIC) gamma = log( sum( myd$NoAtALK)) / 2;
  }
  
  m <- tryCatch.W.E( gam(f,data=myd,family="binomial",weights=NoAtALK,gamma=gamma,...) )$value
  if(class(m)[2]=="error") { print(m); stop("Error occured for age ",a,"\n","Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n");}
  if(verbose) { print(summary(m)); } 
  return(m);
}


##' @title Fit a continuation-ratio logit model for age given length and possibly other covariates. 
##' @param x a DATRASraw object.
##' @param minAge minimum age group to consider
##' @param maxAge maximum age group to consider
##' @param mc.cores The number of cores to use, i.e. how many processes will be spawned (at most)
##' @param model Model formula(string) for ALK, or a vector of strings specifying the formula for each logit (i.e. number of age groups minus one).
##' @param method Use default formula: 1=Normal distributed age-at-length (1st order), 2=Second order polynomial, 3=Spatial ALK, first order length-effect.
##' @param autoChooseK Automatic choice of the max. dimension for the basis used to represent the smooth term for spatial ALK. See ?s in the mgcv-package.
##' @param useBIC Use Bayesian Information Criterion for smoothness selection instead of AIC.
##' @param varCof Use varying coefficients model for spatial effect.
##' @param maxK Maximum k to use. Only applies if autoChooseK is TRUE. 
##' @param gamma Multiplier for AIC score (see ?gam)
##' @param verbose Print details about the fitting process.
##' @param ... Optional extra arguments to gam()
##' @return An object of class 'ALKmodel'
fitALK<-function(x,minAge,maxAge,mc.cores=1,model= c( "cra~LngtCm", "cra~poly(LngtCm,2)", "cra~LngtCm+s(lon,lat,bs='ts')" )[method],
                        method=1,autoChooseK=FALSE,useBIC=FALSE,varCof=FALSE,maxK=100,gamma=1.4,verbose=FALSE,...){
  checkSpectrum(x);
  if( (minAge+1)>maxAge ) stop("Invalid age selection.");
  ages=minAge:maxAge;
  nAges=length(ages);
  ## remove last selected age group, as it is automatically added as plus group later
  lastAge=ages[nAges];
  ages=ages[-nAges];

  extraVars=unlist(  lapply( lapply(model,as.formula),xtraVars,x=x) );
  if(length(extraVars)==0) extraVars=NULL;
  
  ##merge covariates from hydro data to age data by haul id.
  x[[1]]=merge(x[[1]],x[[2]][c("lon","lat","haul.id",extraVars)],by="haul.id",all.x=TRUE,sort=FALSE,suffixes=c("",".y"))
  x[[1]]=subset(x[[1]],!is.na(Year) & !is.na(Age) & !is.na(LngtCm))
  
  mylapply<-function(...){
    hasmc=(mc.cores>1 && require(multicore,quietly=TRUE));
    if(!hasmc) return(lapply(...)) else return(mclapply(...,mc.cores=mc.cores))
  }
  if(verbose) cat("Fitting model...");
  models = mylapply(ages,fitALKone,ages=ages,AL=x[[1]],model=model,gamma=gamma,autoChooseK=autoChooseK,useBIC=useBIC,varCof=varCof,maxK=maxK,verbose=verbose,...);
  class(models)<-"ALKmodel";
  attr(models,"data")<-x;
  attr(models,"ALKformula")<-model;
  attr(models,"ages")<-ages;
  models
}

##' @title Predict method for ALKmodel objects
##' @param x An object of class 'ALKmodel'
##' @param newdata optionally, a DATRASraw object to predict 
##' @param type the type of prediction required. The default is "Nage", which is numbers-at-age for each haul. The other option is "ALK", which gives a list of age-length keys, one for each haul. 
##' @param mc.cores use this number of cores (parallel computation via multicore library)
##' @return A matrix if type equals "Nage", and a list of matrices if type equals "ALK".
predict.ALKmodel<-function(object,newdata=NULL,type="Nage",mc.cores=1,...){
  x <- object
  dat=attr(x,"data");
  if(!is.null(newdata)){
      dat=newdata;
      checkSpectrum(dat)
      attr(x,"data")<-dat;
    }
  ages=attr(x,"ages");
  len=attr(dat,"cm.breaks")[1:ncol(dat$N)];
  N=length(len);
  mylapply<-function(...){
    hasmc=(mc.cores>1 && require(multicore,quietly=TRUE));
    if(!hasmc) return(lapply(...)) else return(mclapply(...,mc.cores=mc.cores))
  }
  if(type=="Nage"){
    Nage=mylapply(1:nrow(dat[[2]]),NageByHaul,x=x)
    n3=matrix(unlist(Nage),nrow(dat[[2]]),length(Nage[[1]]),byrow=TRUE);
    lastAge=max(ages)+1;
    colnames(n3)<-c( as.character(ages), paste(lastAge,"+",sep=""));
    return(n3);
  } else if(type=="ALK"){
    ALK=mylapply(1:nrow(dat[[2]]),NageByHaul,x=x,returnALK=TRUE)
    ##n3=array(unlist(ALK),c(nrow(dat[[2]]),N,length(ages)+1 ) )
    ##colnames(n3)<-c( as.character(ages), paste(lastAge,"+",sep=""));
    return(ALK);
  } else stop("Unknown type");
}

##' @title Calculate numbers-at-age for a particular haul.
##' @param row Row number (haul) to be predicted. 
##' @param x An object of class "ALKmodel"
##' @param returnALK Return ALK instead of numbers-at-age
##' @return Vector with numbers-at-age (or ALK)
NageByHaul<-function(row,x,returnALK=FALSE){
  dat=attr(x,"data");
  ##checkSpectrum(dat);
  ##if( is.null( attr( dat, "ALKmodels")) ) stop("No ALK model found in data object. Use function 'addNage' for that");
  ##models=attr(dat,"ALKmodels");
  models=x;
  extraVars=unlist( lapply( lapply(attr(models,"ALKformula"),as.formula),xtraVars,x=dat) );
  if(length(extraVars)==0) extraVars=NULL;
  maxAge=length(models)+1;
  len=attr(dat,"cm.breaks")[1:ncol(dat$N)];
  N=length(len);
  cc=1:N;

  nd=data.frame(LngtCm=len,lat=dat[[2]][row,"lat"],lon=dat[[2]][row,"lon"]);
  nd[,extraVars]=dat[[2]][row,extraVars];
  p = matrix(1,nrow=N,ncol=maxAge);
    
  punc<-function(k,a){
    p[k,a]*prod(1-p[k,1:(a-1)]);
  }
  
  W = getOption("warn") 
  options(warn=-1);  ## disable warnings temporarily
  for(i in 1:(maxAge-1)){
    p[,i] = 1-predict(models[[i]],newdata=nd,type="response",newdata.guaranteed=TRUE);
  }
  options(warn=W) ## restore warnings

  predProps=p;
  for(a in 2:maxAge)
    {
        ## unconditional prob of age_i = Pi_i * Prod(1-Pi_j , j=1..i-1) for i>1. [Rindorf,Lewy p.2]
        predProps[,a] = sapply(cc,punc,a=a);
      }
   
  if(!returnALK) { return(dat[[2]]$N[row,]%*%predProps); } else { return( predProps); }
}

##' Add numbers-at-age to a DATRASraw object.
##' Numbers are estimated using a continuation-ratio logit model.
##' This is just a short-cut for calling 'fitALK' to fit the model followed by 'predict.ALKmodel' and
##' adding numbers-at-age per haul to the DATRASraw object in a variable called 'Nage'.
##' 
##' @title Add numbers-at-age to a DATRASraw object.
##' @param x a DATRASraw object.
##' @param ages Vector with consecutive ages to consider, last age is plus group.
##' @param mc.cores The number of cores to use, i.e. how many processes will be spawned (at most)
##' @param model Model formula(string) for ALK, or a vector of strings specifying the formula for each logit (i.e. number of age groups minus one).
##' @param method Use default formula: 1=Normal distributed age-at-length (1st order), 2=Second order polynomial, 3=Spatial ALK, first order length-effect.
##' @param autoChooseK Automatic choice of the max. dimension for the basis used to represent the smooth term for spatial ALK. See ?s in the mgcv-package.
##' @param useBIC Use Bayesian Information Criterion for smoothness selection instead of AIC.
##' @param varCof Use varying coefficients model for spatial effect.
##' @param maxK Maximum k to use. Only applies if autoChooseK is TRUE. 
##' @param gamma Multiplier for AIC score (see ?gam)
##' @param verbose Print details about the fitting process.
##' @param ... Optional extra arguments to gam()
##' @return A DATRASraw object.
addNage<-function(x,ages,mc.cores=1,model= c( paste("cra~LngtCm"), paste("cra~poly(LngtCm,2)"), paste("cra~LngtCm+s(lon,lat,bs='ts')"))[method],
                        method=1,autoChooseK=FALSE,useBIC=FALSE,varCof=FALSE,maxK=100,gamma=1.4,verbose=FALSE,...){
  checkSpectrum(x);
  nAges=length(ages);
  if(nAges<2 || sum(diff(ages)==1)!=(nAges-1)) { stop("Invalid age selection.");}
  
  ALK=fitALK(x,min(ages),max(ages),mc.cores,model,method,autoChooseK,useBIC,varCof,maxK,gamma,verbose,...)
  Nage=predict(ALK);
  names(dimnames(Nage))[1] <- "haul.id"
  names(dimnames(Nage))[2] <- "ageGroup"
  rownames(Nage) <- rownames(x$N)
  x$Nage=Nage;
  x
}

##' @title Compute approximate likelihood ratio test for the equality of two age-length keys. 
##' @param object An object of class 'ALKmodel'
##' @param object2 An object of class 'ALKmodel'
##' @return a p-value.
anova.ALKmodel<-function(object,object2,...){
  m1 <- object
  m2 <- object2
  
  getEdf<-function(m) sum(m$edf)
  myll<-function(m) logLik(m)
  ll1=sum( unlist(  lapply(m1,myll)))
  edfs1=sum( unlist( lapply(m1,getEdf)))

  ll2=sum( unlist(  lapply(m2,myll)))
  edfs2=sum( unlist( lapply(m2,getEdf)))

  cat("logLik 1: ",ll1, " logLik 2: ",ll2," edf1:",edfs1," edf2: ",edfs2,"\n");
  if(edfs2>edfs1){
    1-pchisq( 2*(ll2-ll1), edfs2-edfs1);
  } else {
    1-pchisq( 2*(ll1-ll2), edfs1-edfs2);
  }

}

##' @title Compute Akaike Information Criterion for an age-length key model.
##' @param x an ALKmodel object.
##' @param k Penalty for number of parameters 
##' @return AIC value.
AIC.ALKmodel<-function(object,..., k=2){
  x <- object
  getEdf<-function(m) sum(m$edf)
  myll<-function(m) logLik(m)
  ll=sum( unlist(  lapply(x,myll)))
  edfs=sum( unlist( lapply(x,getEdf)))

  -2*ll + k*edfs;
}
##' @title Plot a raw ALK using the observed proportions in each age group. 
##' @param x a DATRASraw object with added spectrum.
##' @param minAge pool all ages less than or equal to this age.
##' @param maxAge pool all ages greater than or equal to this age.
##' @param truncAt truncate proportions below this number for nicer plots
##' @param type argument to matplot()
##' @param ylab argument to matplot()
##' @param xlab argument to matplot()
##' @param ... extra parameters to matplot()
##' @return nothing
plotALKraw<-function(x,minAge,maxAge,truncAt=0,type="l",ylab="Proportion",xlab="Cm",...){
  checkSpectrum(x);
  cm.breaks=attr(x,"cm.breaks");
  x[[1]]$sizeGroup <- cut(x[[1]]$LngtCm,breaks=cm.breaks,right=FALSE)
  x[[1]]$AgeA <- x[[1]]$Age;
  x[[1]]$AgeA[x[[1]]$AgeA < minAge] = minAge;
  x[[1]]$AgeA[x[[1]]$AgeA > maxAge] = maxAge;
  tab = xtabs(NoAtALK~sizeGroup+AgeA,data=x[[1]]);
  propTab=tab/rowSums(tab)
  propTab[propTab<truncAt]=NA;
  matplot(cm.breaks[-length(cm.breaks)],propTab,type=type,ylab=ylab,xlab=xlab,...)
}
##' @title plot fitted ALK for a given haul
##' @param x An object of class 'ALKmodel'
##' @param row row number in hydro data (i.e. haul number)
##' @param lwd argument to matplot()
##' @param type argument to matplot()
##' @param ylab argument to matplot()
##' @param xlab argument to matplot()
##' @param ... extra parameters for matplot()
##' @return nothing
plotALKfit<-function(x,row,lwd=2,type="l",ylab="Proportion",xlab="Cm",...){
  dat=attr(x,"data");
  checkSpectrum(dat);
  cm.breaks=attr(dat,"cm.breaks");
  ALK = NageByHaul(row,x,returnALK=TRUE);
  matplot(cm.breaks[-length(cm.breaks)],ALK,lwd=lwd,type=type,ylab=ylab,xlab=xlab,...);
}

checkSpectrum<-function(x){
    if( is.null( attr( x, "cm.breaks")) ) stop("No spectrum found on DATRASraw object. Use function 'addSpectrum' for that");
}


## We want to catch *and* save both errors and warnings, and in the case of
## a warning, also keep the computed result.
##
## @title tryCatch both warnings and errors
## @param expr expression to evaluate
## @return a list with 'value' and 'warning', where
##   'value' may be an error caught.
## @author Martin Maechler
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
         warning = w.handler),
       warning = W)
}
