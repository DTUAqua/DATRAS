## ---------------------------------------------------------------------------
## Read ICES data and convert to .RData
## Remember that ICES use "-9" to code missing values !
## ---------------------------------------------------------------------------
readICES <- function(file="IBTS.csv",na.strings=c("-9","-9.0","-9.00","-9.0000"),strict=TRUE){
  cat("Locating lines with headers\n")
  print(system.time(lines <- readLines(file)))
  system.time(i <- grep("RecordType",lines))
  skip <- i-1
  nrow <- c(diff(i),0)-1
  cat("Reading data files\n")
  d <- lapply(1:length(i),function(i){
    print(system.time(ans <- read.csv(file,nrow=nrow[i],skip=skip[i],na.strings=na.strings)))
    ans$StNo <- as.character(ans$StNo)
    ans
  })
  ## Data components must come in specific order
  names(d) <- sapply(d,function(x)as.character(x$RecordType[1]))
  d <- d[c("CA", "HH", "HL")]
  cat("Classes of the variables\n")
  print(lapply(d,function(x)sapply(x,class)))
  if(sum(sapply(d,nrow)) + length(i) != length(lines))stop("csv file appears to be corrupt.")
  ## Inconsistencies with variable names are resolved here
  ## =====================================================
  ## Ices-square variable should have the same name ("StatRec") in age and hydro data.
  if(is.null(d[[1]]$StatRec))d[[1]]$StatRec <- d[[1]]$AreaCode
  d <- addExtraVariables(d)
  d <- fixMissingHaulIds(d,strict=strict)
  class(d) <- "DATRASraw"
  d
}

## ---------------------------------------------------------------------------
##' General method for subsetting a DATRASraw object.
##'
##' A DATRASraw object contains two kinds of information:
##' \enumerate{
##' \item Between haul information (contained in component 2)
##' \item Within haul information (contained in component 1 and 3)
##' }
##' This method can subset over both kinds of information - at once.
##' The functionality is achieved by matching each of the subset critera
##' to the appropriate data component. When a match is found the subset is
##' performed on the component and the subsetting is continued to the next
##' subset criteria. The number of subset critera can be arbitrary and is
##' given through the \code{...} argument.
##' 
##' @title Subsetting a DATRASraw object.
##' @param x Take subset of this dataset.
##' @param ... One or more subset criteria.
##' @param na.rm Discard missing values in subset criterion?
##' @return The reduced dataset.
##' @method subset DATRASraw
##' @S3method subset DATRASraw
##' @examples
##' \dontshow{
##' file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
##' }
##' x <- readExchange(file1)
##' y <- subset(x,Species=="Gadus morhua",LngtCm>30,HaulDur>25)
## ---------------------------------------------------------------------------
subset.DATRASraw <- function(x,...,na.rm=TRUE){
  old.nrow <- sapply(x,nrow) ## To test what parts of x have been changed
  args <- as.list(match.call()[-1][-1])
  vars <- lapply(args,all.vars)
  na2false <- function(x){
    if(!na.rm)return(x)
    if(!is.logical(x))stop("na2false requires logicals")
    x[is.na(x)] <- FALSE
    x
  }
  for(i in seq(args)){
    var <- vars[[i]]
    arg <- args[[i]]
    fit <- sapply(x,function(x)any(var %in% names(x)))
    if(!any(fit)){
      cat("Warning - no match found for:\n")
      print(arg)
    }
    if(fit[2]){ ## ===>> haul.id subset
      x[[2]] <- x[[2]][na2false(eval(arg,x[[2]],parent.frame())),]
      lev <- levels(factor(x[[2]]$haul.id))
      x[[1]] <- x[[1]][x[[1]]$haul.id %in% lev,]
      x[[3]] <- x[[3]][x[[3]]$haul.id %in% lev,]
    } else { ## d1 , d3 subset
      if(fit[1])x[[1]] <- x[[1]][na2false(eval(arg,x[[1]],parent.frame())),]
      if(fit[3])x[[3]] <- x[[3]][na2false(eval(arg,x[[3]],parent.frame())),]
    }
  }
  ## Remove empty factor levels
  new.nrow <- sapply(x,nrow)
  changed <- old.nrow!=new.nrow
  ## d1,d3 --> refactor but not haul.id.
  refactor13 <- function(df){
    i <- sapply(df,is.factor)
    i <- i & (names(df)!="haul.id")
    df[i] <- lapply(df[i],factor)
    df
  }
  ## d2 -->
  ## * refactor all of d2.
  ## * refactor haul.id in d1,d3 consistently
  refactor2 <- function(df){
    i <- sapply(df[[2]],is.factor)
    df[[2]][i] <- lapply(df[[2]][i],factor)
    lev <- levels(df[[2]]$haul.id)
    df[[1]]$haul.id <- factor(df[[1]]$haul.id,levels=lev)
    df[[3]]$haul.id <- factor(df[[3]]$haul.id,levels=lev)
    df
  }
  for(i in c(1,3))if(changed[i])x[[i]] <- refactor13(x[[i]])
  if(changed[2])x <- refactor2(x)
  x
}


print.DATRASraw <- function(x,...){
  d1 <- x[[1]]
  d2 <- x[[2]]
  d3 <- x[[3]]
  cat("Object of class 'DATRASraw'\n")
  cat("===========================\n")
  cat("Number of hauls:",nrow(d2),"\n")
  cat("Number of species:",nlevels(d3$Species),"\n")
  cat("Number of countries:",nlevels(d2$Country),"\n")
  cat("Years:",levels(d2$Year),"\n")
  cat("Quarters:",levels(d2$Quarter),"\n")
  cat("Gears:",levels(d2$Gear),"\n")
  numNa <- sum(is.na(d2$HaulDur))
  cat(paste("Haul duration:",paste(unique(range(d2$HaulDur,na.rm=TRUE)),collapse=" - "),
            "minutes"))
  if(numNa){cat(" (");cat(numNa,"NAs removed)")}
  cat("\n")
}

"$.DATRASraw" <- function(x,name)x[[2]][[name,exact=FALSE]]
"$<-.DATRASraw" <- function(x,name,value){x[[2]][[name]] <- value;x}

##' Sample unit subset for DATRASraw object.
##'
##' Extract all information related to a given set of hauls.
##' For instance x[1:2] extracts the first two hauls.
##' Duplicated integer values results in automatic renaming of haul ids.
##' This can be useful for sampling with replacement.
##' @title Sample unit subset.
##' @param x DATRASraw object
##' @param i Integer vector
##' @return DATRASraw object
##' @rdname indexSubset
##' @method [ DATRASraw
##' @S3method [ DATRASraw
##' @examples
##' \dontshow{
##' file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
##' x <- readExchange(file1)
##' }
##' x[1:2]
##' x[sample(3,replace=TRUE)]
##' split(x,x$Country)
"[.DATRASraw" <- function(x,i){
  if(is.character(i))return(x[[2]][i])
  if(any(duplicated(i))){
    ## Special workaround for duplicated case:
    warning("Subset with haul duplication - hauls are renamed")
    keep <- as.character(x[[2]]$haul.id)[i]
    ind <- lapply(x,function(x)seq.int(length.out=nrow(x)))
    hid <- lapply(x,function(y)factor(y$haul.id,levels=levels(x[[2]]$haul.id)))
    splind <- Map(split,ind,hid)
    splhid <- Map(split,lapply(hid,as.character),hid)
    indnew <- lapply(splind,"[",keep)
    hidnew <- lapply(splhid,"[",keep)
    hidapp <- lapply(hidnew,function(x)rep(seq(keep),sapply(x,length)))
    indfinal <- lapply(indnew,unlist)
    hidfinal <- Map(paste, lapply(hidnew,unlist), hidapp, sep=".")
    ans <- lapply(1:3,function(i)x[[i]][indfinal[[i]],,drop=FALSE])
    for(k in 1:3)ans[[k]]$haul.id <- factor(hidfinal[[k]],levels=hidfinal[[2]])
    class(ans) <- "DATRASraw"
    return(ans)
  }
  ## Following code assumes i is unique
  haul.levels <- as.character(x$haul.id[i])
  call <- substitute(subset(x,haul.id %in% levels),list(levels=haul.levels))
  eval(call)
}
length.DATRASraw <- function(x)nrow(x[[2]])

summary.DATRASraw <- function(object, ...){
  x <- object
  print(x);
  cat("Number of hauls by year and quarter:\n")
  print(xtabs( ~ Year + Quarter, data=x[[2]]));
  cat("Haul duration:\n")
  print(summary(x$HaulDur))
}

## ---------------------------------------------------------------------------
##' Unpack and read raw exchange zipfile.
##'
##' The raw exchange files from ICES are zipfiles (without zip extension).
##' This function unzips the file to a temporary directory and then reads
##' and converts the resulting text file.
##' @title Read exchange data into R.
##' @param zipfile File to read.
##' @param strict if TRUE, missing haul ids in age data should be unqiuely matched when filled in, if FALSE a random match will be assigned.
##' @return DATRASraw object.
readExchange <- function(zipfile,strict=TRUE){
  tempdir <- tempdir()
  csvfile <- unzip(zipfile,exdir=tempdir)[1]
  cat("Processing csv file:\n")
  print(csvfile)
  readICES(csvfile,strict=strict)
}

## ---------------------------------------------------------------------------
##' Read all exchange files in a folder and combine to a single DATRASraw
##' object
##'
##' Calls \code{readExchange} on all the exchange files in the folder and
##' combines the results.
##' @title Read all exchange files in a folder.
##' @param path File path. 
##' @param pattern Pattern that the exchange files match.
##' @param strict if TRUE, missing haul ids in age data should be unqiuely matched when filled in, if FALSE a random match will be assigned.
##' @return DATRASraw object
readExchangeDir <- function(path=".",pattern=".zip",strict=TRUE){
  zipfiles <- dir(path=path,pattern=pattern,recursive=TRUE,full.names=TRUE)
  all <- lapply(zipfiles,readExchange,strict=strict)
  do.call("c",all)
}

## ---------------------------------------------------------------------------
##' Method to combine many datasets
##'
##' This function combines DATRASraw objects by \code{rbind}ing each of the
##' three components of the objects.
##' The method is useful when downloading large amounts of data in small chunks.
##' If the same haul.id is present within different datasets the method will stop.
##' If a variable name is only present in some of the datasets, the variable will be
##' added to the remaining datasets (filled with NA) and a warning will be triggered.
##' 
##' @title Combine multiple DATRASraw objects 
##' @param ... DATRASraw objects to put together.
##' @return Merged dataset.
##' @method c DATRASraw
##' @S3method c DATRASraw
##' @examples
##' \dontshow{
##' file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
##' file2 <- system.file("exchange","Exchange3.zip",package="DATRAS")
##' }
##' x <- readExchange(file1)
##' y <- readExchange(file2)
##' z <- c(x,y)
## ---------------------------------------------------------------------------
c.DATRASraw <- function(...){
  testUniqueHaulID <- function(args){
    x <- lapply(args,function(x)levels(x$haul.id))
    if(length(unique(unlist(x))) != length(unlist(x)))stop("Haul ids must be unique.")
  }
  args <- list(...)
  testUniqueHaulID(args)
  args <- lapply(args,unclass) ## because do_mapply dispatch on length
  ## Add missing variable names with warning
  argnames <- Map(Map,list("names"),args)
  union <- function(...)unique(c(...))
  unionNames <- do.call("Map",c(list("union"),argnames))
  addMissingVariables <- function(x){
    out <- lapply(1:3,function(i){
      ans <- x[[i]]
      missingVariables <- setdiff( unionNames[[i]],names(ans) )
      if(length(missingVariables)>0){
        warning("Incomplete DATRASraw? Missing ",names(x)[i],"-record(s): ",
                paste(missingVariables,collapse=", "),
                ". NA will be inserted.")
        ans[missingVariables] <- NA
      }
      ans
    })
    names(out) <- names(x)
    out
  }
  args <- lapply(args,addMissingVariables)
  args <- c(list("rbind"),args)
  ans <- do.call("Map",args)
  ans <- reorderTimeLevels(ans)
  ## Note that rbind drops all zero-row data.frames - hence levels of zero-length
  ## factor will be dropped. Have to fix this:
  ans <- refactorHaulLevels(ans)
  class(ans) <- "DATRASraw"
  ans
}

## ---------------------------------------------------------------------------
## Utility functions used when reading DATRAS data from a file
## ---------------------------------------------------------------------------
addHaulID <- function(d){
  haul.id <- quote( factor(paste(Year,Quarter,Country,Ship,Gear,StNo,HaulNo,sep=":"))  )
  for(i in 1:3)d[[i]]$haul.id <- eval(haul.id,d[[i]])
  d
}
addExtraVariables <- function(IBTS){
  d1 <- IBTS[[1]] ## Age data
  d2 <- IBTS[[2]] ## Hydro data - one line for each haul
  d3 <- IBTS[[3]] ## Length data
  ## ---------------------------------------------------------------------------
  ## Interpretation of ICES codes
  ## ---------------------------------------------------------------------------
  ## LngtCode interpretation: The BITS manual states that this code
  ## identifies the *interval* of the size measurement. E.g. code "5" associated
  ## with LngtClass=65 corresponds to size interval 65cm-70cm.
  ## For units smaller than 1cm (i.e. "." and "0") we also have to move the decimal point,
  ## e.g. LngtClass=500 with "." corresponds to 50.0cm-50.1cm.
  LngtCode2cm <- c("."=0.1, "0"=0.1, "1"=1, "2"=1, "5"=1) ## Valid for IBTS. Also BITS ? - YES!
  file <- system.file("SpeciesTable.csv",package="DATRAS")
  specdat <- read.csv(file,skip=3,strip.white = TRUE)
  SpecCode2species <- structure(as.character(specdat$Species), names=specdat$TSN.code)

  file <- system.file("WoRMSTable.csv",package="DATRAS")
  specdat <- read.csv(file)
  WSpecCode2species <- structure(as.character(specdat$ScientificName_WoRMS),
                                 names=specdat$WoRMS_AphiaID)

  lookup <- function(x,table){
    x <- factor(x)
    levels(x) <- table[levels(x)]
    if(is.numeric(table)) as.numeric(as.character(x)) else x
  }
  mytransform <- function(d3){
    d3 <- transform(d3,
                    LngtCm = lookup(LngtCode,LngtCode2cm) * d3$LngtClas, ## "d3$LngtClass" corresponds to "d1$LngtClas" !?
                    ##Species = lookup(SpecCode,SpecCode2species)
                    Species = factor(
                      ifelse(as.character(SpecCodeType)=="W",
                             as.character( lookup(SpecCode,WSpecCode2species) ),
                             as.character( lookup(SpecCode,SpecCode2species) )
                      )
                      )
                    )
    d3
  }
  d3 <- mytransform(d3)
  d1 <- mytransform(d1)
  haul.id <- quote( factor(paste(Year,Quarter,Country,Ship,Gear,StNo,HaulNo,sep=":"))  )
  d1$haul.id <- eval(haul.id,d1)
  d2$haul.id <- eval(haul.id,d2)
  d3$haul.id <- eval(haul.id,d3)
  
  ## Reconstruct the original count-variable (ICES have standardized to 1 hour).
  ## DataType:
  ## C: corrected to 1 hour 
  ## R: raw
  ## S: ICES February 2012:
  ##    DATRAS with data type S by present time are misinterpreted by the data submitters and
  ##    are not different from the submissions with data type R.
  ## HLNoAtLngt for DataTypes R and S should be multiplied with SubFactor!
  d3 <- merge(d3,d2[c("haul.id","HaulDur","DataType")],by="haul.id",all.x=TRUE,sort=FALSE)
  multiplier <- ifelse(d3$DataType=="C",d3$HaulDur/60,d3$SubFactor)
  d3$Count <- d3$HLNoAtLngt*multiplier
  
  d2$abstime <- local(Year+(month-1)*1/12+(Day-1)/365,d2)
  d2$timeOfYear <- local((month-1)*1/12+(Day-1)/365,d2)
  d2$TimeShotHour=as.integer(d2$TimeShot/100) + (d2$TimeShot%%100)/60;
  d2 <- transform(d2,lon=ShootLong,lat=ShootLat)

  ## Add roundfish area numbers
  file <- system.file("roundfish.csv",package="DATRAS")
  rf <- read.table(file)
  d2$Roundfish=NA;
  for(r in 1:nrow(rf))  d2$Roundfish[ d2$StatRec==as.character(rf[r,2]) ] = rf[r,1];
  d2$Roundfish=as.factor(d2$Roundfish);
 
  ## ---------------------------------------------------------------------------
  ## Allow some exceptions (with warning) 
  ## ---------------------------------------------------------------------------
  diff <- setdiff(levels(d2$haul.id),levels(d3$haul.id)) ## Hauls for which length is missing
  if(length(diff)>0){
    cat("========= WARNING: ============\n")
    cat("Hauls without length info will be interpreted as empty hauls:\n")
    print(diff)
  }

  ## Identical haul levels
  d1$haul.id <- factor(d1$haul.id,levels=levels(d2$haul.id))
  d3$haul.id <- factor(d3$haul.id,levels=levels(d2$haul.id))
  
  ## ---------------------------------------------------------------------------
  ## "haul.id" consistent with Hydro data ? 
  ## ---------------------------------------------------------------------------
  stopifnot(nlevels(d3$haul.id) == nrow(d2))
  ##stopifnot(identical(levels(h3),levels(h2)))
  cat("Consistency check passed\n")

  
  IBTS[[1]] <- d1
  IBTS[[2]] <- d2
  IBTS[[3]] <- d3

  ## Convert Year and Quarter to factor
  for(i in 1:3){
    IBTS[[i]]$Year <- factor(IBTS[[i]]$Year)
    IBTS[[i]]$Quarter <- factor(IBTS[[i]]$Quarter)
  }
  
  IBTS
}

reorderTimeLevels <- function(x){
  for(i in 1:3){
    x[[i]]$Year <- factor(x[[i]]$Year,levels=sort(levels(x[[i]]$Year)))
    x[[i]]$Quarter <- factor(x[[i]]$Quarter,levels=sort(levels(x[[i]]$Quarter)))
  }
  x
}

refactorHaulLevels <- function(df){
  lev <- levels(df[[2]]$haul.id)
  df[[1]]$haul.id <- factor(df[[1]]$haul.id,levels=lev)
  df[[3]]$haul.id <- factor(df[[3]]$haul.id,levels=lev)
  df
}

## ---------------------------------------------------------------------------
## Method for filling in missing haul ids in age data
## ---------------------------------------------------------------------------
fixMissingHaulIds<-function(d,strict=TRUE){
  if(!any(is.na(d[[1]]$haul.id))) return(d);

  d1=d[[1]][is.na(d[[1]]$haul.id),]
  noNA=nrow(d1)
    
  d2=d[[2]][,c("StatRec","Year","Quarter","Country","Ship","haul.id","Roundfish")];  
  d2$StatRec=as.character(d2$StatRec);
  d1$AreaCode=as.character(d1$AreaCode);
  d1$StatRec=d1$AreaCode

  dtmp=d1[,-which(names(d1)=="haul.id")]
  dtmp$rowno=1:nrow(dtmp)

  dm=merge(dtmp,d2,all.x=TRUE,by=c("StatRec","Year","Quarter","Country","Ship"),sort=FALSE,suffixes=c("",".y"))
  tab=table(dm$rowno);
  if(length(tab)!=noNA) stop("something went wrong in FixMissingHaulIds");
  uniqueRows=as.numeric(names(tab)[tab==1]);
  nonUnique=as.numeric(names(tab)[tab>1]);
  if(  (length(uniqueRows)+length(nonUnique))!=noNA) stop("something went wrong in FixMissingHaulIds 2");

  dm=dm[sample(1:nrow(dm)),]; ## permute to get some more random assignments when duplicates are dropped
  dm=dm[!duplicated(dm$rowno),]
  dm=dm[order(dm$rowno),];
  matchedUniquely=sum(!is.na(dm$haul.id))-length(nonUnique);
  if(strict){
    if(length(nonUnique>0)) dm[nonUnique,"haul.id"]=NA;
    d[[1]][is.na(d[[1]]$haul.id),]$haul.id=dm$haul.id;
    newnoNA=sum(is.na(d[[1]]$haul.id));
    if(length(nonUnique>0)) warning(paste(noNA,"age data haul ids missing:",matchedUniquely,"entries could be matched uniquely to a haul, the rest (",newnoNA,") will be left as 'NA' and dropped by any subsequent subsetting"));
    return(d);
  }
  
  nomatch=dm[is.na(dm$haul.id),]
  ##cat(nrow(nomatch), " are still unmatched - using roundfish area instead of rectangle\n")
  if(nrow(nomatch)>0){
    dm=dm[!is.na(dm$haul.id),]

    sel=which(nchar(nomatch$StatRec)==1)
    nomatch$Roundfish=as.character(nomatch$Roundfish);
    nomatch$Roundfish[sel]=nomatch$StatRec[sel]
    
    dm2=merge(nomatch,d2,all.x=TRUE,by=c("Roundfish","Year","Quarter","Country","Ship"),sort=FALSE,suffixes=c(".x",""))
    dm2=dm2[sample(1:nrow(dm2)),]; ## permute to get some more random assignments when duplicates are dropped
    dm2=dm2[!duplicated(dm2$rowno),]

    result=rbind( dm[,c("rowno","haul.id")],dm2[,c("rowno","haul.id")])
  } else result=dm[,c("rowno","haul.id")];
  
  result=result[order(result$rowno),]

  d[[1]][is.na(d[[1]]$haul.id),]$haul.id=result$haul.id;

  newnoNA=sum(is.na(d[[1]]$haul.id));
  if(length(nonUnique>0)) warning(paste(noNA,"age data haul ids missing:",matchedUniquely,"entries could be matched uniquely to a haul,",noNA-matchedUniquely-newnoNA," were assigned a random haul amongst matching candidates,", newnoNA," could not be matched any haul.\nUnmatched hauls were left as 'NA' and will be dropped by any subsequent subsetting."));

  return(d);
}


## ---------------------------------------------------------------------------
## Method for adding the length spectrum to the DATRASraw format
## Compact format is used, i.e. dataframes with numbers in each length category are added to d2.
## ---------------------------------------------------------------------------
addSpectrum <- function(x,cm.breaks=seq(min(x[[3]]$LngtCm,na.rm=TRUE),max(x[[3]]$LngtCm,na.rm=TRUE)+by,by=by),
                            by=getAccuracyCM(x))
  {
    stopifnot(class(x)=="DATRASraw");
    if(length(levels(x[[3]]$Species))>1) warning("Multiple species found - spectrum will contain all species");
    if(any( x[[2]]$DataType=="S")) warning("DataType 'S' found in length data. These hauls will be interpreted as DataType 'R' wrt. total numbers caught.")
    x[[3]]$sizeGroup <- cut(x[[3]]$LngtCm,breaks=cm.breaks,right=FALSE)
    N <- xtabs(Count ~ haul.id + sizeGroup , data=x[[3]])
    N <- round(N)
    x[[2]]$N <- N[as.character(x[[2]]$haul.id),,drop=FALSE]
    attr(x,"cm.breaks")<-cm.breaks;
    x
  }
getAccuracyCM <- function(x){
  ch <- as.character(x[[3]]$LngtCode)
  LngtCode2cm <- c("."=0.1, "0"=0.5, "1"=1, "2"=2, "5"=5)
  y <- LngtCode2cm[ch]
  ans <- max(y,na.rm=TRUE)
  if(length(na.omit(unique(y)))>1)warning(paste("Mixed accuracies found in var[[3]]$LngtCode - worst chosen:",ans,"cm"))
  if(any(is.na(unique(y))))warning(paste("NAs found in var[[3]]$LngtCode - assumed to be",ans,"cm"))
  ans
}

## ---------------------------------------------------------------------------
##' Method to convert DATRASraw to data.frame with one line for each response.
##'
##' This function should be called as the final step after
##' preprocessing ( subset(), addSpectrum(), addNage(), etc ).
##' The output format can be controlled by the argument \code{format}.
##' If format="long" the data.frame has one line for each response which
##' is the standard for most statistical procedures.
##' In the less usual case format="wide" the data.frame has one line for
##' each haul (sample id) with multiple responses across the columns of the
##' response matrix. The wide format thus takes up less memory.
##' If multiple response names are available in \code{x} (e.g. both "N" and
##' "Nage") then the user must select one through the argument \code{response}.
##' 
##' @title Convert DATRASraw to data.frame suitable for statistical analysis.
##' @param x DATRASraw object to be converted.
##' @param format Long or wide format?
##' @param response Name of response variable (only needed if data has multiple
##' response variables). 
##' @param cleanup Remove useless information? 
##' @param ... Not used.
##' @return data.frame with one line for each response along with associated
##' covariates.
##' @method as.data.frame DATRASraw
##' @S3method as.data.frame DATRASraw
## ---------------------------------------------------------------------------
as.data.frame.DATRASraw <- function(x, ..., format=c("long","wide"),response,
                                    cleanup=TRUE){
  format <- match.arg(format)
  ## Add species information
  x$Species <- paste(levels(factor(x[[3]]$Species)),collapse="|")
  x$Species <- factor(x$Species)
  x <- x[[2]]
  if(cleanup){ ## Remove useless information  
    rownames(x) <- NULL
    x$RecordType <- NULL
    allNA <- sapply(x,function(x)all(is.na(x)))
    if(any(allNA))warning("Empty columns removed... ",
                          paste(names(allNA[allNA]),collapse=", "))
    x <- x[!allNA]
  }
  if(format=="wide")return(x)
  i <- sapply(x,is.matrix)
  y <- x[i]   ## data.frame with response variables
  x <- x[!i]  ## data.frame without response variables
  nm <- names(y)
  if(!missing(response))y <- y[response]
  if(length(y)!=1){
    stop("response variable must be one of: \n",
         paste(nm,collapse=" "),"\n")
  }
  asLong <- function(responseName){
    mat <- y[[responseName]]
    df <- data.frame(factor(colnames(mat)[col(mat)],
                            levels=colnames(mat)),    ## e.g.: new size/age variable
                     as.vector(mat))                  ## new count variable
    ch <- names(dimnames(mat))[2]
    if(is.null(ch)){
      warning("Response matrix does not have a named column dimension (e.g. ageGroup or sizeGroup)")
      ch <- ""
    }
    names(df)[1] <- ch
    names(df)[2] <- responseName
    ## Expand x:
    i <- as.vector(row(mat))
    cbind(x[i,],df)
  }
  asLong(names(y))
}

##' Plot map with haul positions and ICES squares.
##' If a response variable is present also plot frequencies within each square.
##'
##' @title Plot map with haul positions.
##' @param x DATRASraw object.
##' @param add Plot on top of existing plot?
##' @param pch Point type of positions.
##' @param plot.squares Add ICES squares?
##' @param plot.map Add a map?
##' @param plot.points Plot haul positions?
##' @param plot.response Plot frequencies of response if present?
##' @param xlim Longitude range of plot.
##' @param ylim Latitude range of plot.
##' @param ... Controlling plot of positions.
##' @method plot DATRASraw
##' @S3method plot DATRASraw
##' @examples
##' \dontshow{
##' file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
##' }
##' x <- readExchange(file1)
##' y <- subset(x,lon>9,LngtCm<60)
##' plot(y,col="red")
##' ##' Add response variable
##' y <- addSpectrum(y)
##' plot(y,col="red")
plot.DATRASraw <- function(x,add=FALSE,pch=16,
                           plot.squares=!add,
                           plot.map=!add,
                           plot.points=TRUE,
                           plot.response=TRUE,
                           xlim=NULL,
                           ylim=NULL,
                           ...){
  ## Experiment
  if(FALSE){
    ## Overwrite: icesSquare(), icesSquare2coord(), x[[2]]$StatRec
    sq <- names(pol)
    rglon <- lapply(pol,function(x)range(x$lon))
    rglat <- lapply(pol,function(x)range(x$lat))
    pollist <- Map(function(x,y)data.frame(lon=x[c(1,1,2,2)],lat=y[c(1,2,2,1)]),rglon,rglat)
    
  }
  ## End experiment
  sq <- unique(icesSquare(x))
  pol <- icesSquare2coord(sq,"polygons")
  point <- icesSquare2coord(sq,"midpoint")
  range <- as.data.frame(lapply(do.call("rbind",pol),range))
  if(!is.null(xlim))range$lon[] <- xlim
  if(!is.null(ylim))range$lat[] <- ylim
  if(!add)plot(range,type="n",las=1,xlab="Longitude",ylab="Latitude")
  if(plot.squares){
    pol2 <- do.call("rbind",lapply(pol,function(x)rbind(x,NA)))
    polygon(pol2, col = "lightgrey")
    text(point$lon,point$lat,names(pol))
  }
  if(plot.points)points(x$lon,x$lat,pch=pch,...)
  if(plot.map){
    require(maps)
    require(mapdata)
    map("worldHires",add=TRUE,lwd=1,col="darkgrey",fill=FALSE)
  }
  if(plot.response){
    response <- which(sapply(x[[2]],is.matrix))[1]
    if(!is.na(response)){
      statfun <- function(x){z <- colMeans(x);z/max(z)}
      yy <- lapply(split(x[[2]][response], x$StatRec), statfun)
      plotfun <- function(sq){
        y <- yy[[sq]]
        x <- seq(y)
        tilePlot({
          points(x-.5,y,type="h",lwd=2)
        },c(0,max(x)),c(0,1),pol=pol[[sq]])
      }
      sapply(names(pol),plotfun)
    }
  }
  NULL
}
## Utility function for plot.DATRASraw
## expr={commands to do a plot}
## xlim and ylim of this plot.
## Fit the plot within polygon "pol"
##' Add new plot on existing plot.
##'
##' Add new plot on existing plot.
##' @title Add small plot on existing plot.
##' @param expr Expression of plot commands producing new plot.
##' @param xlim New plot x-range.
##' @param ylim New plot y-range.
##' @param pol Polygon defining the sub-window wrt. large plot coordinate system.
##' @return Output of evaluated \code{expr}
tilePlot <- function(expr,xlim,ylim,pol){
  usr.old <- par("usr")
  on.exit(par(usr=usr.old))
  usr.pol <- unlist(lapply(pol,range))
  scalex <- diff(xlim)/diff(usr.pol[1:2])
  scaley <- diff(ylim)/diff(usr.pol[3:4])
  step <- usr.old-usr.pol
  usr.new <- step*(c(scalex,scaley)[c(1,1,2,2)]) + c(xlim,ylim)
  par(usr=usr.new)
  expr
}
##' @title Positions can be added to existing plot.
##' @rdname plot.DATRASraw
##' @method points DATRASraw
##' @S3method points DATRASraw
points.DATRASraw <- function(x,...){
  plot(x,add=TRUE,...)
}

##' Add spatial data from a shapefile to a DATRASraw object.
##' Longitude-latitude coordinates from DATRASraw are used to
##' lookup the information in the shapefile. Requires the package
##' \code{maptools}.
##'
##' @title Add spatial data from shapefile.
##' @param d DATRASraw object
##' @param shape Shapefile or valid filename of existing shapefile.
##' @param select Vector of variable names to select from shapefile (default is to select all names)
##' @param ... Passed to \code{readShapeSpatial}
##' @return Modified DATRASraw object
addSpatialData <- function(d,shape,select=NULL,...){
  require(maptools)
  if(is.character(shape)){
    if(file.exists(shape)){
      shape <- readShapeSpatial(shape)
    }
  }
  tmp <- d[[2]]
  coordinates(tmp) <- ~lon+lat
  xtra <- over(tmp, shape)
  if(!is.null(select))xtra <- xtra[select]
  nm <- names(xtra)
  if(length(intnm <- intersect(names(d[[2]]),nm))){
    print(intnm)
    stop("Some selected names from shapefile are already in DATRASraw")
  }
  d[[2]] <- cbind(d[[2]],xtra)
  d
}
