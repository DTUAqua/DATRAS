\name{weightAtAge}
\alias{weightAtAge}
\title{Calculate weight by age group in the survey accounting for length-stratified subsampling of age.}
\usage{
  weightAtAge(d, minAge, maxAge,
    alk.type = c("raw", "smooth"))
}
\arguments{
  \item{d}{DATRASraw object}

  \item{minAge}{minimum age group to consider}

  \item{maxAge}{maximum age group (plus group)}
}
\value{
  A vector with weight-at-age
}
\description{
  Calculate weight by age group in the survey accounting
  for length-stratified subsampling of age.
}
\details{
  Empirical application of Bayes formula.
}

