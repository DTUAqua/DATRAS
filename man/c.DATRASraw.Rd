\name{c.DATRASraw}
\alias{c.DATRASraw}
\title{Combine multiple DATRASraw objects}
\usage{
  c.DATRASraw(...)
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
  amounts of data in small chunks.
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

