#download all of the scripts
library(xml2)

baseurl <- "http://live.adventuretracking.com/xml/"

race_name <- "bayviewmack"

years <- 2012:2018

n <- 400

urls <- paste0(baseurl, race_name, years, "?n=", n)
filenames <- paste0(race_name, years, ".xml")

for (i in seq_along(years)){
  
  test <- xml2::download_html(urls[i], file = filenames[i])
  
}

