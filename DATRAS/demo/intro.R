require(DATRAS)

## Example file
zipfile <- system.file("exchange","Exchange1.zip",package="DATRAS")

## Step 1. Read exchange data into R
d <- readExchange(zipfile)

## Step 2. Preprocess the data
## -Take subset
## -Add size spectrum
d <- subset(d,lon>10,Species=="Gadus morhua")
d <- addSpectrum(d)
d <- addNage(d,2:4)

## Step 3. Convert to data.frame with one line for each response.
df <- as.data.frame(d,response="Nage")

