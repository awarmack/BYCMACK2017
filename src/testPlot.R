#Test plot of two boats

#get the data
source("./src/parseXML.R")


#library calls
library(ggplot2)

dat <- rbind(zub, alb)


ggplot(boatdat, aes(x=Longitude, y=Latitude)) + geom_point(aes(color=boat))


library(ggmap)
library(ggrepel)

map_lims <- c(min(dat$Latitude), min(dat$Latitude), max(dat$Longitude), max(dat$Latitude)) 

lkhuron <- get_map(location = "Lake Huron", zoom=7)

ggmap(lkhuron) + 
  geom_point(data=dat, aes(x=Longitude, y=Latitude, color=boat)) + 
  scale_x_continuous(limits = c(-85, -82))+
  scale_y_continuous(limits = c(43, 46))
