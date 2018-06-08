library(DATRAS)

## Tools
md5sums.old <- readLines("md5sums")
cat("", file="md5sums")
checksum <- function(x, expected=NULL) {
    xname <- deparse(substitute(x))
    file <- tempfile()
    on.exit(file.remove(file))
    cat(x, file=file)
    ans <- tools::md5sum(file)
    cat(xname, " ", ans, "\n", file="md5sums", append=TRUE)
    NULL
}

## Tests
d <- readExchangeDir(".")

checksum( capture.output(print(d)) )

checksum( levels(d[["HL"]]$Species) )

checksum( names(d) )

checksum( colnames(d[["CA"]]) )

checksum( colnames(d[["HH"]]) )

checksum( colnames(d[["HL"]]) )

checksum(levels(d$haul.id))

ds <- subset(d, Species=="Gadus morhua")
checksum( capture.output(print(ds)) )

ds <- addSpectrum(ds)
checksum(ds$N)

ds <- addNage(ds, ages=0:6)
checksum(ds$Nage)

spl <- split(d, d$Quarter)
checksum( capture.output(print(spl)) )

## Check:
md5sums.new <- readLines("md5sums")
stopifnot(identical(md5sums.new, md5sums.old))
message("TESTS PASSED :)")
