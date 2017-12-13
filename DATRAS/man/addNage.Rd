\name{addNage}
\alias{addNage}
\title{Add numbers-at-age to a DATRASraw object.}
\usage{
  addNage(x, ages, mc.cores = 1,
    model = c(paste("cra~LngtCm"), paste("cra~poly(LngtCm,2)"), paste("cra~LngtCm+s(lon,lat,bs='ts')"))[method],
    method = 1, autoChooseK = FALSE, useBIC = FALSE,
    varCof = FALSE, maxK = 100, gamma = 1.4,
    verbose = FALSE, ...)
}
\arguments{
  \item{x}{a DATRASraw object.}

  \item{ages}{Vector with consecutive ages to consider,
  last age is plus group.}

  \item{mc.cores}{The number of cores to use, i.e. how many
  processes will be spawned (at most)}

  \item{model}{Model formula(string) for ALK, or a vector
  of strings specifying the formula for each logit (i.e.
  number of age groups minus one).}

  \item{method}{Use default formula: 1=Normal distributed
  age-at-length (1st order), 2=Second order polynomial,
  3=Spatial ALK, first order length-effect.}

  \item{autoChooseK}{Automatic choice of the max. dimension
  for the basis used to represent the smooth term for
  spatial ALK. See ?s in the mgcv-package.}

  \item{useBIC}{Use Bayesian Information Criterion for
  smoothness selection instead of AIC.}

  \item{varCof}{Use varying coefficients model for spatial
  effect.}

  \item{maxK}{Maximum k to use. Only applies if autoChooseK
  is TRUE.}

  \item{gamma}{Multiplier for AIC score (see ?gam)}

  \item{verbose}{Print details about the fitting process.}

  \item{...}{Optional extra arguments to gam()}
}
\value{
  A DATRASraw object.
}
\description{
  Add numbers-at-age to a DATRASraw object. Numbers are
  estimated using a continuation-ratio logit model. This is
  just a short-cut for calling 'fitALK' to fit the model
  followed by 'predict.ALKmodel' and adding numbers-at-age
  per haul to the DATRASraw object in a variable called
  'Nage'.
}

