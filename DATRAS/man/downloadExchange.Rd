\name{downloadExchange}
\alias{downloadExchange}
\title{Interface to DATRAS php download script}
\usage{
  downloadExchange(survey, years = NULL)
}
\arguments{
  \item{survey}{Name of survey to download (For a list of
  available names run \code{downloadExchange()})}

  \item{years}{Vector of years to download. If unspecified,
  all years are downloaded one by one.}
}
\value{
  NULL
}
\description{
  Interface to DATRAS php download script
}
\details{
  Note: Command line php must be installed.
}

