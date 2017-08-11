#Performance analysis vs. Polars

source("./src/loadData.R")
source("./src/loadPolars.R")



library(akima)
library(ggplot2)



# get the polar targets
targets <- interpp(x=pol$BAW, 
                      y=pol$VAW, 
                      z=pol$V, 
                      xo = perf$AWA, 
                      yo= perf$AWS,
                      linear=FALSE,
                      extrap=FALSE,
                      duplicate = "mean")

#Calculate Measurables
perf$target.SOG <- targets$z
perf$diff.SOG <- perf$SOG - perf$target.SOG
perf$pol.perc <- (perf$SOG / perf$target.SOG) * 100


#plot the performance
ggplot() + 
  geom_point(data=perf,  aes(x=AWA, y=SOG, color=AWS), alpha=0.7) + 
  coord_polar("x") + 
  scale_x_continuous(limits=c(0,360), breaks=seq(0,360, 15))+
  scale_y_continuous(limits=c(0,8), breaks=seq(0,8, by=2))+
  scale_color_gradientn(colors=rainbow(5), breaks=seq(0,30, 5))+
  ggtitle("Speed Over Ground [SOG] by Apparent Wind Angle and Wind Speed")

ggsave("./figs/01_SOG_by AWA and AWS.png", width=10, height=10, units="cm", scale=2)




#histogram of Apparent Wind Angle
ggplot(perf, aes(AWA, fill=AWS.range)) + 
  geom_histogram(binwidth = 15, position = "stack") + 
  coord_polar() + 
  scale_x_continuous(limits=c(0,360), breaks=seq(0,360,15), name = "Apparent Wind Angle [AWA]")+
  scale_color_discrete(name="AWS Range [kts]")+
  ggtitle("Distribution of Apparent Wind & Direction ")

ggsave("./figs/02_Distribution Apparent Wind and Direction.png", width=10, height=10, units="cm", scale=2)



#plot the path
library(ggmap)
map_lims <- c(min(perf$LON), min(perf$LAT), max(perf$LON), max(perf$LAT)) 

lkhuron <- get_map(location = map_lims, zoom=8)

ggmap(lkhuron) + 
  geom_path(data=perf, aes(x=LON, y=LAT, color=SOG), size=2) +
  scale_color_gradientn(colors=rainbow(5), limits=c(0,10), breaks=seq(0,10, 2))

ggmap(lkhuron) + 
  geom_path(data=perf, aes(x=LON, y=LAT, color=pol.perc), size=2)+
  scale_color_gradient2(midpoint=100, mid="blue", name="% of Polar")


#Speed over time

t_lims <- c(as.POSIXct("2017-07-22 20:00:00"), as.POSIXct("2017-07-24 04:00:00"))

ggplot(perf, aes(x=TIME.M, y=SOG, color=SOG)) + geom_path() +
  ggtitle("GPS Speed Over Ground")

ggplot(perf) + 
  geom_path(aes(x=TIME.M, y=SOG, color=pol.perc), size=1, lineend="round") +
  ggtitle("GPS Speed Over Ground vs. Target")+
  scale_color_gradientn(colors=rainbow(5), breaks=seq(0,120, by=20), name= "% of Target")+
  scale_x_datetime(breaks=date_breaks("2 hours"), date_labels = "%a %H:%M")+
  geom_path(aes(x=TIME.M, y=target.SOG, linetype="Target SOG"))+scale_linetype(name="Polars")





#histogram of difference vs. Target
ggplot(perf, aes(pol.perc)) + geom_histogram(col="black") + scale_x_continuous(breaks=seq(0,150, by=15))
  

pol_labels <- pol[pol$BTW==180, ]

#difference vs. Polar
ggplot() + 
  geom_path(data=pol, aes(x=BAW, y=V, group=VTW)) + 
  coord_polar("x") + 
  scale_x_continuous(limits=c(0,360), breaks=seq(0,360, 15), name="Apparent Wind Angle AWA") + 
  scale_y_continuous(name="SOG")+
  geom_vline(xintercept=0)+
  geom_vline(xintercept=90)+
  geom_vline(xintercept=180)+
  geom_vline(xintercept=270)+
  ggtitle("% of Polar Performance by AWA")+
  geom_point(data=perf, aes(x=AWA, y=SOG, color=pol.perc))+
  scale_color_gradientn(colors=rainbow(5), breaks=seq(0,120, by=20), name= "% of Target")+
  geom_text(data=pol_labels, aes(x=BTW, y=V, label=VTW), hjust=1)


ggplot(perf, aes(x=TIME.M, y=SOG, color=pol.perc)) + geom_point() + 
  scale_color_gradientn(colors=rainbow(5), breaks=seq(30, 120, 20))+
  geom_path(aes(x=TIME.M, y=target.SOG), color="black")


ggplot(perf)+geom_histogram(aes(pol.perc), bins=30, color="black")+scale_x_continuous(breaks=seq(0, 150, by=10))+
  geom_vline(col="red", xintercept=mean(perf$pol.perc, na.rm = TRUE))

ggplot(perf)+geom_histogram(aes(pol.perc, fill=AWS.range), bins=30, color="black")+scale_x_continuous(breaks=seq(0, 150, by=10))+
  geom_vline(col="red", xintercept=mean(perf$pol.perc, na.rm = TRUE))+facet_grid(AWS.range ~ .)
