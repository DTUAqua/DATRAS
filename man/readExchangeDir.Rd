\name{readExchangeDir}
\alias{readExchangeDir}
\title{Read all exchange files in a folder.}
\usage{
  readExchangeDir(path = ".", pattern = ".zip")
}
\arguments{
  \item{path}{File path.}

  \item{pattern}{Pattern that the exchange files match.}
}
\value{
  DATRASraw object
}
\description{
  Read all exchange files in a folder and combine to a
  single DATRASraw object
}
\details{
  Calls \code{readExchange} on all the exchange files in
  the folder and combines the results.
}

