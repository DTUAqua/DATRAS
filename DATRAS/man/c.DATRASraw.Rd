\name{c.DATRASraw}
\alias{c.DATRASraw}
\title{Combine multiple DATRASraw objects}
\usage{
  \method{c}{DATRASraw} (...)
}
\arguments{
  \item{...}{DATRASraw objects to put together.}
}
\value{
  Merged dataset.
}
\description{
  Method to combine many datasets
}
\details{
  This function combines DATRASraw objects by
  \code{rbind}ing each of the three components of the
  objects. The method is useful when downloading large
  amounts of data in small chunks. If the same haul.id is
  present within different datasets the method will stop.
  If a variable name is only present in some of the
  datasets, the variable will be added to the remaining
  datasets (filled with NA) and a warning will be
  triggered.
}
\examples{
\dontshow{
file1 <- system.file("exchange","Exchange1.zip",package="DATRAS")
file2 <- system.file("exchange","Exchange3.zip",package="DATRAS")
}
x <- readExchange(file1)
y <- readExchange(file2)
z <- c(x,y)
}

