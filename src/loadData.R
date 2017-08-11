#summardize by time
library(dplyr)
library(tidyr)
options(scipen=999)

#Summarise by second, 5 seconds
rm(list=ls())

load("./alldat.RDATA")


#summarise by minute

alldat$TIME.M <- as.POSIXct(round(alldat$TIME.S, "mins"))
alldat$TIME.S <- as.POSIXct(round(alldat$TIME.S, "secs"))

#get performance data

perf <- alldat %>% group_by(TIME.M) %>% summarise(SOG=mean(GPS.SOG, na.rm = TRUE), 
                                                  AWS=mean(AWS, na.rm = TRUE), 
                                                  AWA=mean(AWA, na.rm = TRUE), 
                                                  COG=mean(GPS.COG, na.rm=TRUE), 
                                                  LAT=mean(GGA.LAT, na.rm=TRUE),
                                                  LON=mean(GGA.LON, na.rm=TRUE))

perf$AWA[perf$AWA > 180] <- 360 - perf$AWA[perf$AWA > 180] 
perf <- perf[perf$AWA > 0, ] 


#break AWS into bins
perf$AWS.range <- cut(perf$AWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24, 35))
#perf$AWS.range <- gsub("]", ")", perf$AWS.range)
#perf$AWS.range <- gsub(",", "~", perf$AWS.range)

#perf$AWS.range <- ordered(perf$AWS.range, levels=c("(0~6)", "(6~8)", "(8~10)", "(10~12)", "(12~16)", "(16~20)", "(20~24)", "(24~35)"))

#break AWA into bins 
perf$AWA.range <- cut(perf$AWA, breaks=seq(0,180, by=15))



#convert AWS, AWA, and SOG to TWS and TWD


getTrueWind <- function(AWS, AWA, SOG) { 
  #outputs a list with 
  # $tws = True Wind Speed
  # $twa = True Wind Angle
  
  b <- AWA * (pi/180)
  
  TWS <- sqrt(AWS^2 + SOG^2 - 2*AWS*SOG*cos(b))
  
  # from polars: 
  #VTW = SQRT( (VAW * SIN(BAW))^2 + (VAW x cos(BAW)-Vboat)^2)
  #
  # VTW = velocity true wind
  # VAW = Velocity Apparent
  # BAW = Bearing of Apparent Wind
  #
  
  #TWS <- sqrt( (AWS * sin(b))^2 + (AWS * cos(b)-SOG)^2)
  
  
  
  #BTW = arc tan ( (VAW x sin(BAW)) / (VAW x cos(BAW)-vboat))
  
  
  TWA <- atan( (AWS * sin(b))/ (AWS * cos(b)-SOG))
  TWA <- TWA*180/pi
    
  
  #calcTWA <- function(b, AWS.b, SOG.b, W){
  #   if(b > 0 & b<2*pi) {
  #     
  #     TWA <- acos(((AWS.b * cos(b)) - SOG.b)/W)
  #     
  #   } else {
  #     if(b > 2*pi) {
  #       TWA <- NA
  #     }
  #     
  #     TWA <- -acos(AWS.b * cos(b) - SOG.b)
  #     
  #   }
  #   
  #   if(!is.na(TWA)){
  #     
  #     TWA <- TWA*180/pi
  #     
  #   }
  #   return(TWA)
  # }
  
  #TWA <- mapply(FUN=calcTWA, b, AWS, SOG, TWS)

  x <- list(TWS=TWS, TWA=TWA)
  
  
  }

tw <- getTrueWind(perf$AWS, perf$AWA, perf$SOG)
perf$TWS <- tw$TWS
perf$TWA <- tw$TWA


#add point of sail to perf


waypoints <- alldat %>% filter(!is.na(BWC.WPT)) %>% group_by(BWC.WPT) %>% summarise(LAT = mean(WPT.LAT),
                                                                                  LON = mean(WPT.LON))




