library(RNetCDF)

setwd("C:/users/Awarmack/Dropbox/Projects/Sailing/BYCMACK2017")

filename <-paste0("./dat/", list.files("./dat")[5])

dat <- open.nc(filename)
datrd <- read.nc(dat)
