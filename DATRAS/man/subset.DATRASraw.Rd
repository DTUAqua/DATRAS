\name{subset.DATRASraw}
\alias{subset.DATRASraw}
\title{Subsetting a DATRASraw object.}
\usage{
  \method{subset}{DATRASraw} (x, ..., na.rm = TRUE)
}
\arguments{
  \item{x}{Take subset of this dataset.}

  \item{...}{One or more subset criteria.}

  \item{na.rm}{Discard missing values in subset criterion?}
}
\value{
  The reduced dataset.
}
\description{
  General method for subsetting a DATRASraw object.
}
\details{
  A DATRASraw object contains two kinds of information:
  \enumerate{ \item Between haul information (contained in
  component 2) \item Within haul information (contained in
  component 1 and 3) } This method can subset over both
  kinds of information - at once. The functionality is
  achieved by matching each of the subset critera to the
  appropriate data component. When a match is found the
  subset is performed on the component and the subsetting
  is continued to the next subset criteria. The number of
  subset critera can be arbitrary and is given through the
  \code{...} argument.
}
\examples{
\dontshow{
file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
}
x <- readExchange(file1)
y <- subset(x,Species=="Gadus morhua",LngtCm>30,HaulDur>25)
}

