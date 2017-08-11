#load polars

#load raw data
polars <- read.csv("./dat/polars/fullpolar.csv")

#remove optimum points
opt <- polars[grep("OPT", polars$SAIL), ]
polars <- polars[-grep("OPT", polars$SAIL),  ]

#get the maximum V (velocity) for each wind speed and angle
pol <- polars %>% group_by(VTW, BTW) %>% summarise(V=max(V))

#rejoin to the sail and heel data
pol <- left_join(pol, polars)


#create function to give expected speed given AWA and AWS




