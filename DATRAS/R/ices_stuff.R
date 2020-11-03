## ---------------------------------------------------------------------------
##' Calculate Longitude/Latitude pairs from ICES square.
##'
##' Output from this function depends on argument \code{format}:
##' \enumerate{
##' \item format="corner": Return south-west corner of square.
##' \item format="midpoint": Return midpoint of square.
##' \item format="polygons": Return list of square polygons (suitable for
##' \code{polygon} plot function).
##' }
##' @title ICES square to Longitude/Latitude conversion.
##' @param x Vector of ICES square names.
##' @param format Return points or polygons?
##' @return data.frame with coordinates or list of polygons.
##' @examples
##' icesSquare2coord(c("37G1","37G2"))
##' icesSquare2coord(c("37G1","37G2"),"polygons")
##' @export
## ---------------------------------------------------------------------------
icesSquare2coord <- function (x,format=c("corner","midpoint","polygons")) 
{
  format <- match.arg(format)
  ch12num <- structure(1:98,names=formatC(1:98,width=2,flag="0"))
  ch34num <- structure(1:113,names=c(paste("A",0:3,sep=""),head(as.vector(t(outer(LETTERS[2:13][-8],0:9,paste,sep=""))),-1)))
  lat <- .5*(ch12num[substring(x, 1, 2)]+71)
  lon <- ch34num[substring(x, 3, 4)]-45
  df <- function(lon,lat){
    ans <- data.frame(lon,lat)
    rownames(ans) <- x
    ans
  }
  if(format=="corner")return(df(lon,lat))
  if(format=="midpoint")return(df(lon+.5,lat+.25))
  fun <- function(lon,lat) {
    expand.grid(lon = c(lon, lon + 1), lat = c(lat, lat + 0.5))[c(1, 2, 4, 3), ]
  }
  ans <- Map(fun,lon,lat)
  names(ans) <- x
  ans
}

## ---------------------------------------------------------------------------
##' Calculate ICES square from Longitude/Latitude pairs. 
##' @title Longitude/Latitude to ICES square conversion.
##' @param data A DATRASraw object or a data.frame with Longitude/Latitude
##' information.
##' @param lon Vector of longitudes.
##' @param lat Vector of latitudes.
##' @return Vector of ICES square names.
##' @export
## ---------------------------------------------------------------------------
icesSquare <- function (data, lon = data$lon, lat = data$lat) 
{
  ch12 <- formatC(1:98,width=2,flag="0")
  ch34 <- c(paste("A",0:3,sep=""),head(as.vector(t(outer(LETTERS[2:13][-8],0:9,paste,sep=""))),-1))
  x <- floor(lon+45)
  y <- floor(2 * lat - 71)
  paste(ch12[y], ch34[x], sep = "")
}
