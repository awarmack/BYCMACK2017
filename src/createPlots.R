#plot figs

load("./dat/alldat.Rdata")

library(ggplot2)
library(zoo)

ggplot(alldat, aes(x=TIME.S, y=GPS.SOG))  + geom_point(alpha=0.3) 

ggplot(alldat, aes(x=TIME.S, y=GPS.SOG))  + geom_point(alpha=0.3)


x <- zoo(alldat, order.by = index(1))


#Plots

#SOG for entire Race
#average over n minutes

#SOG vs. STW


# AWA & AWS, vs. SOG
perf <- alldat %>% group_by(TIME.M) %>% summarise(SOG=mean(GPS.SOG, na.rm = TRUE), 
                                                  AWS=mean(AWS, na.rm = TRUE), 
                                                  AWA=mean(AWA, na.rm = TRUE))



perf$pos <- cut(perf$AWA, breaks=seq(0,360, 15))
levels(perf$pos)


# add point of sails
# "nogo", "close-hull", "close-reach", "beam-reach", "broad-reach", "downwind"




ggplot(perf) + geom_point(aes(x=AWA, y=SOG, color=AWS)) +
  scale_x_continuous(limits=c(0,360), breaks=seq(0,360, 30)) + 
  scale_y_continuous(limits=c(0,8))+
  scale_color_gradientn(colours=rainbow(5), breaks=seq(0,30, 5))+
  coord_polar(theta = "x")

ggplot(perf, aes(x=TIME.M, y=SOG, color=AWS)) + geom_point() + scale_color_gradientn(colors=rainbow(5), breaks=seq(0,30, 5))

#Boat Heading


ggplot(perf) + geom_point(aes(x=AWS, y=SOG))






