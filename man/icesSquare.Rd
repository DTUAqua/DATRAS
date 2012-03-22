\name{icesSquare}
\alias{icesSquare}
\title{Longitude/Latitude to ICES square conversion.}
\usage{
  icesSquare(data, lon = data$lon, lat = data$lat)
}
\arguments{
  \item{data}{A DATRASraw object or a data.frame with
  Longitude/Latitude information.}

  \item{lon}{Vector of longitudes.}

  \item{lat}{Vector of latitudes.}
}
\value{
  Vector of ICES square names.
}
\description{
  Calculate ICES square from Longitude/Latitude pairs.
}

