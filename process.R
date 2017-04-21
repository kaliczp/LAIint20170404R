LAI <- read.csv("LAIday.csv")
library(xts)
LAI.xts <- xts(LAI[,2], as.Date(LAI[,1]))
library(ncdf4)
P.nc <- nc_open("fresee2.1_prec.nc")
Lon <- ncvar_get(P.nc, "Lon")
Lat <- ncvar_get(P.nc, "Lat")

library(sp)
hun <- readRDS("HUN_adm0.rds")
plot(hun)
points(16.453970,47.656329)
points(rep(Lon,length(Lat)),rep(Lat,each=length(Lon)),pch=".")

plot(hun, xlim=c(16.3,16.6), ylim=c(47.5,47.9))
points(16.453970,47.656329)
points(rep(Lon,length(Lat)),rep(Lat,each=length(Lon)))

points(Lon[3],Lat[12],col="purple")

P.hv <- ncvar_get(P.nc,"Precipitation", c(3,12,1),c(1,1,23360))
akt.time <- seq(as.Date("1951-01-01"),as.Date("2014-12-31"),"days")
## Where leap year remove 31st of December
foresee.time <- akt.time[-(grep("-02-29",akt.time)+307)]
Pfre.xts <- xts(P.hv, foresee.time)
plot(Pfre.xts)

dummy.xts <- xts(rep(NA,length(akt.time)), akt.time)
P.xts <- merge.xts(Pfre.xts, dummy.xts)
P.xts <- P.xts[,1]

## Yearly average LAI
LAI.ordered <- LAI[order(substr(LAI[,1],6,10)),]
LAI.aggr <- aggregate(LAI.ordered[,2], list(monthday=substr(LAI.ordered[,1],6,10)), mean)
LAI.aggr.xts <- xts(LAI.aggr[,2],as.Date(paste0("2016-",LAI.aggr[,1])))
plot(LAI.aggr.xts)

## Visualise result
plot(LAI.xts)
lines(LAI.aggr.xts,col=2)
for(tti in 2002:2015)
    lines(xts(LAI.aggr[,2],as.Date(paste(tti,LAI.aggr[,1],sep="-"))),col=2)

##
