\name{readExchange}
\alias{readExchange}
\title{Read exchange data into R.}
\usage{
  readExchange(zipfile)
}
\arguments{
  \item{zipfile}{File to read.}
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

