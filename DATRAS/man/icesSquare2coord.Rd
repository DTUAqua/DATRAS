\name{icesSquare2coord}
\alias{icesSquare2coord}
\title{ICES square to Longitude/Latitude conversion.}
\usage{
  icesSquare2coord(x,
    format = c("corner", "midpoint", "polygons"))
}
\arguments{
  \item{x}{Vector of ICES square names.}

  \item{format}{Return points or polygons?}
}
\value{
  data.frame with coordinates or list of polygons.
}
\description{
  Calculate Longitude/Latitude pairs from ICES square.
}
\details{
  Output from this function depends on argument
  \code{format}: \enumerate{ \item format="corner": Return
  south-west corner of square. \item format="midpoint":
  Return midpoint of square. \item format="polygons":
  Return list of square polygons (suitable for
  \code{polygon} plot function). }
}
\examples{
icesSquare2coord(c("37G1","37G2"))
icesSquare2coord(c("37G1","37G2"),"polygons")
}

