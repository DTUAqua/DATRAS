\name{bayes}
\alias{bayes}
\title{Calculate conditional expectation of some function of length given age.}
\usage{
  bayes(pal, pl, fl)
}
\arguments{
  \item{pal}{matrix with age-length key, i.e. p(A|L).
  Columns correspond to age groups.}

  \item{pl}{vector with length distribution, i.e. p(L).}

  \item{fl}{vector of function values at length, i.e.
  f(L).}
}
\value{
  vector of expectations
}
\description{
  Calculate conditional expectation of some function of
  length given age.
}
\details{
  Find E(f(L)|A) in terms of p(L), p(A|L) and f(L), using
  Bayes formula.
}

