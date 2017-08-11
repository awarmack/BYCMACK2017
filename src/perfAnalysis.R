#Performance analysis vs. Polars
library(dplyr)
library(tidyr)

source("./src/loadData.R")
source("./src/loadPolars.R")



options(scipen=999)

getwd()
#Summarise by second, 5 seconds
rm(list=ls())



#summarise by minute

alldat$TIME.M <- as.POSIXct(round(alldat$TIME.S, "mins"))
alldat$TIME.S <- as.POSIXct(round(alldat$TIME.S, "secs"))

#summarise Performance Data by Minute
perf <- alldat %>% group_by(TIME.M) %>% summarise(SOG=mean(GPS.SOG, na.rm = TRUE), 
                                                  AWS=mean(AWS, na.rm = TRUE), 
                                                  AWA=mean(AWA, na.rm = TRUE), 
                                                  COG=mean(GPS.COG, na.rm=TRUE), 
                                                  LAT=mean(GGA.LAT, na.rm=TRUE),
                                                  LON=mean(GGA.LON, na.rm=TRUE))

#remove any over 180 and convert 
perf$AWA[perf$AWA > 180] <- 360 - perf$AWA[perf$AWA > 180] 
perf <- perf[perf$AWA > 0, ] 

#convert Apparent to True
getTrueWind <- function(AWS, AWA, SOG) { 
  #outputs a list with 
  # $tws = True Wind Speed
  # $twa = True Wind Angle
  
  b <- AWA * (pi/180)
  
  #true wind speed
  TWS <- sqrt(AWS^2 + SOG^2 - 2*AWS*SOG*cos(b))
  
  
  calcTWA <- function(b, AWS.b, SOG.b, W){
    if(b > 0 & b<2*pi) {
      
      TWA <- acos(((AWS.b * cos(b)) - SOG.b)/W)
      
    } else {
      if(b > 2*pi) {
        TWA <- NA
      }
      
      TWA <- -acos(AWS.b * cos(b) - SOG.b)
      
    }
    
    if(!is.na(TWA)){
      
      TWA <- TWA*180/pi
      
    }
    return(TWA)
  }
  
  TWA <- mapply(FUN=calcTWA, b, AWS, SOG, TWS)
  
  x <- list(TWS=TWS, TWA=TWA)
  
}

tw <- getTrueWind(perf$AWS, perf$AWA, perf$SOG)
perf$TWS <- tw$TWS
perf$TWA <- tw$TWA


#break wind speed into bins: 
perf$AWS.range <- cut(perf$AWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24, 35))
perf$AWS.range <- factor(perf$AWS.range, levels=rev(levels(perf$AWS.range)))
perf$TWS.range <- cut(perf$TWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24, 35))
perf$TWS.range <- factor(perf$TWS.range, levels=rev(levels(perf$TWS.range)))

#break wind angle into bins:
perf$AWA.range <- cut(perf$AWA, breaks=seq(0,180, by=30))
perf$TWA.range <- cut(perf$AWA, breaks=seq(0,180, by=30))

write.csv("./dat/data_byminute.csv", row.names = FALSE)
