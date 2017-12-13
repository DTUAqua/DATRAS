##' Interface to DATRAS php download script
##'
##' Note: Command line php must be installed.
##' @title Interface to DATRAS php download script
##' @param survey Name of survey to download (For a list of available names run \code{downloadExchange()})
##' @param years Vector of years to download. If unspecified, all years are downloaded one by one.
##' @return NULL
downloadExchange <- function(survey,years=NULL){
    phpscript <- system.file("datras.php",package="DATRAS")
    cmd <- paste("php",phpscript)
    if(missing(survey)){
        system(cmd)
        return(NULL)
    }
    cmd <- paste(cmd,survey)
    if(is.null(years)){
        system(cmd)
    } else {
        cmd <- paste(cmd,years)
        sapply(cmd,system)
    }
    return(NULL)
}

