\name{bubblePlot}
\alias{bubblePlot}
\title{Plot bubbles on a map with area proportional to "response".}
\usage{
  bubblePlot(d, response = "HaulWgt", scale = NULL,
    col.zero = "red", pch.zero = "+", ...)
}
\arguments{
  \item{d}{DATRASraw object}

  \item{response}{name of variable to plot}

  \item{scale}{scale size of bubbles}

  \item{col.zero}{color for zero hauls}

  \item{pch.zero}{pch for zero hauls}

  \item{...}{extra arguments to plot}
}
\value{
  nothing
}
\description{
  Plot bubbles on a map with area proportional to
  "response".
}
\details{
  See also addWeightByHaul()
}

