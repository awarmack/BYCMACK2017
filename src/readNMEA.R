##Read raw NMEA data
rm(list=ls())
options(scipen=999)

#Load necessary libraries
library(stringr)
library(dplyr)
library(tidyr)


#load supporting functions
source("./src/functions.R")

# Import raw csv file ==============
rawdat <- read.csv("./dat/datalog3", header=FALSE, stringsAsFactors = FALSE)



# TIME.MSamp Cleanup ===============

#Remove any rows where date column has the inccorrect number of characters
dat <- rawdat[nchar(rawdat$V1)==26, ]  

#extract the TIME.MSamp in seconds
dat$TIME.MS <- str_sub(dat$V1, start=4, end=16) 





## split into different frames =========
 
#extract NMEA Headers
dat$NMEA <- str_extract(dat$V1, "\\$.*")


#MWV - wind speed from Raymarine =======
mwv <- dat[dat$NMEA =="$IIMWV",] %>% 
        select(TIME.MS, V2, V4) %>% 
        rename("AWA"=V2, "AWS"=V4) %>%
        mutate(AWA = as.numeric(AWA), 
               AWS = as.numeric(AWS)) %>%
        filter(AWS < 100) %>%
        na.omit()

#STW - boat speed from Raymarine ====                                                     
stw <- dat[dat$NMEA =="$IIVHW", ] %>% 
        select(TIME.MS, V6) %>% rename("STW"=V6) %>%
        mutate(STW = as.numeric(STW)) %>%
        filter(STW < 13) %>%
        na.omit()


#VLW - boat distance travelled from Raymarine ====
vlw <- dat[dat$NMEA == "$IIVLW", ] %>% 
        select(TIME.MS, V2) %>% 
        rename("CM.MILES"=V2)
vlw$CM.MILES <- as.numeric(vlw$CM.MILES)
vlw$CM.MILES[vlw$CM.MILES > 5220.3] <- NA
vlw$CM.MILES[vlw$CM.MILES < 5069] <- NA

#MTW - Water Temperature from Raymarine ====
mtw <- dat[dat$NMEA == "$IIMTW", ] %>% 
               select(TIME.MS, V2) %>% 
               rename("MTW"=V2) %>% 
               mutate(MTW = as.numeric(MTW)) %>% 
               filter(MTW < 26) %>%
               na.omit()


#GPS BOD - bearing from waypoint to waypoint ====

#bod.m = bearing magnetic
#bod.t = bearing true

bod <- dat[dat$NMEA=="$GPBOD",] %>% 
          select(TIME.MS, V2, V4, V6) %>% 
          rename("BOD.T"=V2, 
                 "BOD.M" =V4, 
                 "BOD.WPT" = V6) %>% 
          mutate(BOD.T =as.numeric(BOD.T), 
                 BOD.M= as.numeric(BOD.M))  %>%
          na.omit()


#GPS BWC - Bearing & Distance to Waypoint ====

#wpt.lat = waypoint latitude
#wpt.long = waypoint longitude
#bwc.t = bearing to waypoint True
#bwc.m = bearing to waypoint Mag
#wpt.dis = Distance to Waypoint (nm)
#wpt = Waypoint Name


bwc <- dat[dat$NMEA=="$GPBWC", ] %>% 
       select(TIME.MS, V3, V5, V7, V9, V11, V13) %>%
       mutate(V3 = convCoord(as.numeric(V3)), 
              V5 = convCoord(as.numeric(V5))*-1, 
              V7 = as.numeric(V7), 
              V9 = as.numeric(V9), 
              V11 = as.numeric(V11)) %>% 
       rename("WPT.LAT" = V3, 
              "WPT.LON" = V5, 
              "BWC.T" = V7, 
              "BWC.M" = V9, 
              "WPT.DIS" = V11, 
              "BWC.WPT" = V13) %>% 
       na.omit()


#GPS GGA - Global Positioning System Fix Data from Garmin ====
gga <- dat[dat$NMEA=="$GPGGA", ] %>% 
          select(TIME.MS, V2, V3, V4, V5, V6, V7, V8, V9, V10, V12) %>% 
          mutate(V3 = convCoord(as.numeric(V3)), 
                 V5 = convCoord(as.numeric(V5))*-1,
                 V7 = as.integer(V7), 
                 V8 = as.integer(V8), 
                 V9 = as.numeric(V9),
                 V10 = as.numeric(V10), 
                 V12 = as.numeric(V12)) %>%
          rename("GPS.TIME"=V2, 
                 "GGA.LAT"=V3,
                 "GGA.LON"=V5,
                 "GPS.QUAL"=V7, 
                 "NO.SAT"=V8, 
                 "PREC"=V9,
                 "ANT.ALT"=V10,
                 "GEO.SEP"=V12) %>% 
          filter(GGA.LAT < 47) %>% 
          filter(GGA.LON < -82.5) %>% 
          filter(ANT.ALT < 400) %>% 
          na.omit()


#Geographic Position from Garmin
#gpgll <- dat[dat$NMEA=="$GPGLL", ]


#GPS XTE - Cross Track error Recommended Minimum Navigation Information B ====

#xte = Cross-Track Error  <-- only thing usefull here
#str.dir = Direction to steer
#wpt.to = TO waypoint
#wpt.fr = FROM waypoint
#wpt.lat = Waypoint Latitude
#wpt.lon = Waypoint Longitude
#dest.rng = range to destination in NM
#dest.ber = bearing to destination

xte <- dat[dat$NMEA=="$GPRMB", ]  %>% 
       select(TIME.MS, V3) %>%
       mutate(V3 = as.numeric(V3)) %>% 
       rename("XTE" = V3) %>% 
       filter(XTE < 10) %>%
       na.omit

#GPS RMC - Recommended Minimum Navigation Information C ====

#gps.SOG = GPS Speed Over Ground
#tmg = Track Made Good

rmc <- dat[dat$NMEA=="$GPRMC", ] %>%
        select(TIME.MS, V2, V8, V9, V10) %>%
        mutate(V8 = as.numeric(V8),
               V9 = as.numeric(V9)) %>%
        rename("RMC.TIME"=V2,
               "RMC.SOG" = V8, 
               "RMC.TMG" = V9,
               "RMC.DATE" = V10) %>%
        filter(RMC.SOG < 14) %>%
        na.omit()
  

#GPS VTG -  Track Made Good and Ground Speed ====
vtg <- dat[dat$NMEA=="$GPVTG", ] %>% 
         select(TIME.MS, V2, V6) %>% 
         mutate(V2 = as.numeric(V2),
                V6 = as.numeric(V6)) %>% 
         rename("GPS.SOG" = V6, 
                "GPS.COG" = V2) %>%
         filter(GPS.COG <= 360) %>%
         filter(GPS.SOG <= 12) %>% 
         na.omit()

#GPS RME - Estimated Error ==== 

rme <- dat[dat$NMEA == "$PGRME", ] %>% 
       select(TIME.MS, V2, V4, V6) %>% 
       mutate(V2 = as.numeric(V2), 
              V4 = as.numeric(V4), 
              V6 = as.numeric(V6)) %>% 
       rename("HPE" = V2, 
              "VPE" = V4, 
              "SPE" = V6) %>%
       na.omit()


# Combine Data frames

datframes <- list(mwv, stw, vlw, mtw, bod, bwc, gga, xte, rmc, vtg, rme) 


alldat <- datframes %>%
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="TIME.MS"), .)







#add date/time stamps
alldat$TIME.MS <- as.numeric(alldat$TIME.MS)
alldat <- arrange(alldat, TIME.MS, desc(TIME.MS))


# Adjust Time & Date to GPS Time ====
  difftime <- head(alldat[!is.na(alldat$RMC.TIME), c("TIME.MS", "RMC.TIME", "RMC.DATE")], 1)
  
  #get actual time from GPS (use RMC because it's the most frequent)
  actual.time <- paste0(difftime$RMC.DATE,"-", difftime$RMC.TIME)
  actual.time <- as.POSIXct(strptime(actual.time, format="%d%m%y-%H%M%S", tz="UTC"))
  
  #get the time from the Rasp Pi for that same observation
  pi.time <- as.POSIXct(difftime$TIME.MS/1000, origin="1970-01-01", tz="UTC")
  
  #calculate the difference
  tdiff <- as.numeric(actual.time)*1000 - as.numeric(pi.time)*1000  #time diff in ms
  

  #apply the difference to alldat
  alldat$TIME.MS <- alldat$TIME.MS + tdiff
  
  #test the start and end time
  st_time <- as.POSIXct((head(alldat$TIME.MS,1))/1000 , origin="1970-01-01")
  end_time <- as.POSIXct(tail(alldat$TIME.MS,1)/1000, origin="1970-01-01")
  
  
  alldat$TIME.S <- as.POSIXct(alldat$TIME.MS/1000, origin="1970-01-01")

#reorder columns
alldat <- select(alldat, TIME.S, everything())

print(st_time)
print(end_time)


write.csv(alldat, "./dat/all_data.csv", row.names=FALSE, na="")

save(file="./dat/alldat.RData", list = c("alldat", "mwv", "stw", "vlw", "mtw", "bod", "bwc", "gga", "xte", "rmc", "vtg", "rme") )




