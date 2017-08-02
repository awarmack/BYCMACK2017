##Read raw nmea data

library(stringr)

setwd("~/Dropbox/Projects/Sailing/BYCMACK2017")

rawdat <- read.csv("./dat/datalog3", header=FALSE, stringsAsFactors = FALSE)






#Remove Weird columns
dat <- rawdat[nchar(rawdat$V1)==26, ]  

#extract the time
timest <- str_sub(dat$V1, start=4, end=16) 
timest <- as.numeric(timest)  #adding 36 hrs
timest <- as.POSIXct(timest/1000, origin="1970-01-01")

st_time <- head(timest, 1)
end_time <- tail(timest, 1)


strptime("071420",  format="%h%M%S", tz="UTC")

#adjust time using last GPS as actual
pitime <- as.POSIXct((1500743797513)/1000, origin="1970-01-01", tz="UTC")
actual <- as.POSIXct("2017-07-24 07:14:20 UTC", tz="UTC")
tdiff <- actual - pitime
timest <- timest + tdiff


dat$timest <- timest

dat$nmea <- str_extract(dat$V1, "\\$.*")
