#Analyze Performance vs. Polars

source("./src/loadData.R")

library(ggplot2)
library(dplyr)
library(tidyr)




#get performance data
perf <- alldat %>% group_by(TIME.M) %>% summarise(SOG=mean(GPS.SOG, na.rm = TRUE), 
                                                  AWS=mean(AWS, na.rm = TRUE), 
                                                  AWA=mean(AWA, na.rm = TRUE))

perf$AWA[perf$AWA > 180] <- 360 - perf$AWA[perf$AWA > 180] 
perf <- perf[perf$AWA > 0, ] 


#break AWS into bins
perf$AWS.range <- cut(perf$AWS, breaks=c(0, 6, 8, 10, 12, 16, 20, 24))




#calculate the predicted boat speed based on polars
polars <- read.csv("./dat/polars/fullpolar.csv")


opt <- polars[grep("OPT", polars$SAIL), ]
polars <- polars[-grep("OPT", polars$SAIL),  ]

polmax <- polars %>% group_by(VTW, BTW) %>% summarise(V=max(V))

polmax <- left_join(polmax, polars)

ggplot() + 
  geom_path(data=polmax, aes(x=BAW, y=V, color=VAW, group=VTW), size=1) + 
  coord_polar("x") + scale_x_continuous(limits=c(0,360)) +
  geom_point(data=perf, aes(x=AWA, y=SOG, color=AWS)) + 
  scale_color_gradientn(colors=rainbow(5), breaks=seq(0,30, 5))





