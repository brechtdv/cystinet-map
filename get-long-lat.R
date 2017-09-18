### EXTRACT LONGITUDE/LATITUDE FROM GOOGLE MAPS LINK
###

## required packages
library(bd)

## extractor function
get_long_lat <-
function() {
  x <- as.character(read_cb()$V1)
  print(
    y <-
      strsplit(strsplit(x, "@", fixed = T)[[1]][2], ",", fixed = T)[[1]][1:2])
  write_cb(t(as.numeric(y)), dec = ",")
}

## copy URL and run this function
get_long_lat()