LAI <- read.csv("LAIday.csv")
library(xts)
LAI.xts <- xts(LAI[,2], as.Date(LAI[,1]))
library(ncdf4)
P.nc <- nc_open("fresee2.1_prec.nc")
Lon <- ncvar_get(P.nc, "Lon")
Lat <- ncvar_get(P.nc, "Lat")

