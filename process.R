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
foresee.time <- akt.time[-grep("-02-29",akt.time)+306]