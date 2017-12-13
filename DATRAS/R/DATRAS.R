##' Read and convert raw data obtained from ICES
##' 
##' Read datras hydrographical data, length data and age data into single data
##' structure. Convert length distributions by haul to age distrubutions by
##' haul using continuation ratio logits.
##' 
##' \tabular{ll}{ Package: \tab DATRAS\cr Type: \tab Package\cr Version: \tab
##' 1.0\cr Date: \tab 2010-06-03\cr License: \tab GPL\cr
##' LazyLoad: \tab yes\cr } 
##' 
##' @name DATRAS-package
##' @aliases DATRAS-package DATRAS
##' @docType package
##' @author Kasper Kristensen and Casper Berg
##' 
##' Maintainer: <kaskr@@imm.dtu.dk> <cbe@@aqua.dtu.dk>
##' @references \url{http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx}
##' @example demo/intro.R
NULL


##' Internal DATRAS Functions
##'
##' Internal DATRAS functions
##'
##' These are not to be called by the user (or in some cases are just 
##' waiting for proper documentation to be written :).
##'
##' @name DATRAS-internal
##' @aliases addExtraVariables addHaulID addSpectrum checkSpectrum fixMissingHaulIds getAccuracyCM length.DATRASraw print.DATRASraw readICES refactorHaulLevels reorderTimeLevels summary.DATRASraw tryCatch.W.E xtraVars $.DATRASraw $<-.DATRASraw
##' @rdname DATRAS-internal
NULL
