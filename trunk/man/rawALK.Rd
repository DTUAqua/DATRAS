\name{rawALK}
\alias{rawALK}
\title{Calculate empirical (raw) age length key.}
\usage{
  rawALK(d, minAge, maxAge)
}
\arguments{
  \item{d}{DATRASraw object}

  \item{minAge}{Minimum age}

  \item{maxAge}{Maximum age (plus group)}
}
\value{
  matrix with ALK
}
\description{
  Calculate empirical (raw) age length key.
}
\details{
  Ages < minAge are excluded. Ages > maxAge are pooled into
  a plus group.
}

