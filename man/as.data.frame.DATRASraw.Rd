\name{as.data.frame.DATRASraw}
\alias{as.data.frame.DATRASraw}
\title{Convert DATRASraw to data.frame suitable for statistical analysis.}
\usage{
  as.data.frame.DATRASraw(x, format = c("long", "wide"),
    response, cleanup = TRUE, ...)
}
\arguments{
  \item{x}{DATRASraw object to be converted.}

  \item{format}{Long or wide format?}

  \item{response}{Name of response variable (only needed if
  data has multiple response variables).}

  \item{cleanup}{Remove useless information?}

  \item{...}{Not used.}
}
\value{
  data.frame with one line for each response along with
  associated covariates.
}
\description{
  Method to convert DATRASraw to data.frame with one line
  for each response.
}
\details{
  This function should be called as the final step after
  preprocessing ( subset(), addSpectrum(), addNage(), etc
  ). The output format can be controlled by the argument
  \code{format}. If format="long" the data.frame has one
  line for each response which is the standard for most
  statistical procedures. In the less usual case
  format="wide" the data.frame has one line for each haul
  (sample id) with multiple responses across the columns of
  the response matrix. The wide format thus takes up less
  memory. If multiple response names are available in
  \code{x} (e.g. both "N" and "Nage") then the user must
  select one through the argument \code{response}.
}

