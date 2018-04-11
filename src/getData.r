#download all of the scripts
library(xml2)

baseurl <- "http://live.adventuretracking.com/xml/"

race_name <- "bayviewmack"

years <- 2012:2018

n <- 400

urls <- paste0(baseurl, race_name, years, "?n=", n)
filenames <- paste0("./dat/", race_name, years, ".xml")

#download all files
for (i in seq_along(years)){
  
  xml2::download_html(urls[i], file = filenames[i])
  
}
