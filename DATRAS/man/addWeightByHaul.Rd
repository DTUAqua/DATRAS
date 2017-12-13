\name{addWeightByHaul}
\alias{addWeightByHaul}
\title{Calculate total biomass by haul and add to HH-records}
\usage{
  addWeightByHaul(d)
}
\arguments{
  \item{d}{DATRASraw object}
}
\value{
  DATRASraw object
}
\description{
  Calculate total biomass by haul and add to HH-records
}
\details{
  Fits W = a*L^b and applies to all length data
  (standardized to one minute). Thus, this only makes sense
  for one species.
}

