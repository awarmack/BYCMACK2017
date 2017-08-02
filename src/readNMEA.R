##Read raw nmea data

library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("./src/functions.R")

#Import raw csv file
rawdat <- read.csv("./dat/datalog3", header=FALSE, stringsAsFactors = FALSE)

#Remove any rows where date column is screwed up
dat <- rawdat[nchar(rawdat$V1)==26, ]  

#extract the time
timest <- str_sub(dat$V1, start=4, end=16) 
timest <- as.numeric(timest)  #adding 36 hrs
timest <- as.POSIXct(timest/1000, origin="1970-01-01")

st_time <- head(timest, 1)
end_time <- tail(timest, 1)


#adjust time using last GPS as actual
pitime <- as.POSIXct((1500743797513)/1000, origin="1970-01-01", tz="UTC")
actual <- as.POSIXct("2017-07-24 07:14:20 UTC", tz="UTC")
tdiff <- actual - pitime
timest <- timest + tdiff

dat$timest <- timest

#extract NMEA Headers

dat$nmea <- str_extract(dat$V1, "\\$.*")


#split into different frames

#wind speed from Raymarine
wind <- dat[dat$nmea =="$IIMWV",]
wind <- wind %>% select(nmea, timest, V2, V4) %>% rename("AWA"=V2, "AWS"=V4)
wind$AWA <- as.numeric(wind$AWA)
wind$AWS <- as.numeric(wind$AWS)
wind$AWS[wind$AWS > 100] <- NA  #set anything over 100 to NA

#boat speed from Rawmarine                                                           
stw <- dat[dat$nmea =="$IIVHW", ]
stw <- stw %>% select(nmea, timest, V6) %>% rename("stw"=V6)
stw$stw <- as.numeric(stw$stw)
stw$stw[stw$stw > 13] <- NA

#boat distance travelled from Raymarine
vlw <- dat[dat$nmea == "$IIVLW", ]
vlw <- vlw %>% select(nmea, timest, V2) %>% rename("cm.miles"=V2)
vlw$cm.miles <- as.numeric(vlw$cm.miles)
vlw$cm.miles[vlw$cm.miles > 5220.3] <- NA
vlw$cm.miles[vlw$cm.miles < 5069] <- NA

#GPS bearing
gpbod <- dat[dat$nmea=="$GPBOD",]
gpbod <- gpbod %>% 
          select(nmea, timest, V2, V4, V6) %>% 
          rename("bear.true"=V2, "bear.mag"=V4, "waypt" = V6) %>% 
          mutate(bear.true =as.numeric(bear.true), 
                 bear.mag = as.numeric(bear.mag))

#GPS - Global Positioning System Fix Data
gpgga <- dat[dat$nmea=="$GPGGA", ] %>% 
          select(nmea, timest, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13) %>% 
          rename("time"=V2, 
                 "lat"=V3,
                 "NS"=V4,
                 "lon"=V5,
                 "EW"=V6,
                 "gps.qual"=V7, 
                 "no.sat"=V8, 
                 "prec"=V9,
                 "ant.alt"=V10,
                 "ant.alt.unit"=V11,
                 "geo.sep"=V12,
                 "geo.sep.unit"=V13)

gpgga$lat <- sapply(gpgga$lat, convCoord)  #convert to xx.xxxx format
gpgga$lon <- sapply(gpgga$lon, convCoord)
gpgga <- gpgga[gpgga$lat<47, ]             #remove outliers
gpgga <- gpgga[gpgga$lon>80, ]             
gpgga$lon <- gpgga$lon * -1                #change longitude to negative (for W)

gpgga$ant.alt <- as.numeric(gpgga$ant.alt)  #conv altitude to number
gpgga$ant.alt[gpgga$ant.alt>400] <- NA      #remove outliers

gpgga$prec <- as.numeric(gpgga$prec)        #conv precision to number

#Geographic Position
gpgll <- dat[dat$nmea=="$GPGLL", ]






