\name{readExchange}
\alias{readExchange}
\title{Read exchange data into R.}
\usage{
  readExchange(zipfile, strict = TRUE)
}
\arguments{
  \item{zipfile}{File to read.}

  \item{strict}{if TRUE, missing haul ids in age data
  should be unqiuely matched when filled in, if FALSE a
  random match will be assigned.}
}
\value{
  DATRASraw object.
}
\description{
  Unpack and read raw exchange zipfile.
}
\details{
  The raw exchange files from ICES are zipfiles (without
  zip extension). This function unzips the file to a
  temporary directory and then reads and converts the
  resulting text file.
}

