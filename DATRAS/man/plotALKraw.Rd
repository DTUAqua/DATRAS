\name{plotALKraw}
\alias{plotALKraw}
\title{Plot a raw ALK using the observed proportions in each age group.}
\usage{
  plotALKraw(x, minAge, maxAge, truncAt = 0, type = "l",
    ylab = "Proportion", xlab = "Cm", ...)
}
\arguments{
  \item{x}{a DATRASraw object with added spectrum.}

  \item{minAge}{pool all ages less than or equal to this
  age.}

  \item{maxAge}{pool all ages greater than or equal to this
  age.}

  \item{truncAt}{truncate proportions below this number for
  nicer plots}

  \item{type}{argument to matplot()}

  \item{ylab}{argument to matplot()}

  \item{xlab}{argument to matplot()}

  \item{...}{extra parameters to matplot()}
}
\value{
  nothing
}
\description{
  Plot a raw ALK using the observed proportions in each age
  group.
}

