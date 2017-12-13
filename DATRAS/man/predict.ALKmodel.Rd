\name{predict.ALKmodel}
\alias{predict.ALKmodel}
\title{Predict method for ALKmodel objects}
\usage{
  \method{predict}{ALKmodel} (object, newdata = NULL,
    type = "Nage", mc.cores = 1, ...)
}
\arguments{
  \item{object}{An object of class 'ALKmodel'}

  \item{newdata}{optionally, a DATRASraw object to predict}

  \item{type}{the type of prediction required. The default
  is "Nage", which is numbers-at-age for each haul. The
  other option is "ALK", which gives a list of age-length
  keys, one for each haul.}

  \item{mc.cores}{use this number of cores (parallel
  computation via multicore library)}

  \item{...}{unused}
}
\value{
  A matrix if type equals "Nage", and a list of matrices if
  type equals "ALK".
}
\description{
  Predict method for ALKmodel objects
}

