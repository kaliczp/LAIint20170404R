LAI <- read.csv("LAIday.csv")
library(xts)
LAI.xts <- xts(LAI[,2], as.Date(LAI[,1]))
plot(lowess(index(LAI.xts), coredata(LAI.xts),f=1/20))
LAI.spline <- smooth.spline(index(LAI.xts), coredata(LAI.xts))
plot(LAI.spline,type="l")

plot.zoo(LAI.xts)
lines(LAI.spline,col=2)
LAI.spline.xts <- xts(LAI.spline$y, index(LAI.xts))


library(ncdf4)
## P.nc <- nc_open("fresee2.1_prec.nc")
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
plot(Pfre.xts, xaxs="i")

Pfrepast.yr.xts <- apply.yearly(Pfre.xts, sum)

Ppast.years <- (196:201)*10
Ppast.place <- (196:201)*10-1950

pdf("precpast.pdf", width=13.7, height=3.7, pointsize=14)
par(mar=c(1.5,3.9,0.6,0.6), las=1, mgp=c(2.9,1,0), lend=2)
barplot(Pfrepast.yr.xts,col="lightblue",xaxs="i", ylim=c(0,1170), yaxs="i", space=0, xaxt="n", ylab="Precipitation [mm]")
## plot.zoo(Pfrepast.yr.xts, typ="h", lwd=4, xaxs="i", yaxs="i", ylim=c(0,950),col="lightblue")
par(mgp=c(1,0.5,0))
axis(1,at=Ppast.place,lab=Ppast.years)
box(lwd=2)
dev.off()

library(ncdf4)
P.nc <- nc_open("fresee2.1_REMO_prec.nc")

ttfile <- "fresee2.1_RACMO2-ECHAM5_prec.nc"
P.nc <- nc_open(ttfile)
P.remohv <- ncvar_get(P.nc,"Precipitation", c(3,12,1),c(1,1,31390))
nc_close(P.nc)
akt.time <- seq(as.Date("2015-01-01"),as.Date("2100-12-31"),"days")
foresee.time <- akt.time[-(grep("-02-29",akt.time)+307)]
#Pfre.remo.xts <- xts(P.remohv, foresee.time)
Pfre.racmo2echam5.xts <- xts(P.remohv, foresee.time)
plot(Pfre.racmo2echam5.xts, xaxs="i")

Pfre.remo.yr.xts <- apply.yearly(Pfre.remo.xts, sum)

Pfuture.years <- (202:209)*10
Pfuture.place <- (202:209)*10-2014

pdf("precremo.pdf", width=18.5, height=3.7, pointsize=14)
par(mar=c(1.5,3.9,0.6,0.6), las=1, mgp=c(2.9,1,0), lend=2)
barplot(Pfre.remo.yr.xts,col="lightblue",xaxs="i", ylim=c(0,1170), yaxs="i", space=0, xaxt="n", ylab="Precipitation [mm]")
## plot.zoo(Pfrepast.yr.xts, typ="h", lwd=4, xaxs="i", yaxs="i", ylim=c(0,950),col="lightblue")
par(mgp=c(1,0.5,0))
axis(1,at=Pfuture.place,lab=Pfuture.years)
box(lwd=2)
dev.off()


dummy.xts <- xts(rep(NA,length(akt.time)), akt.time)
P.xts <- merge.xts(Pfre.xts, dummy.xts)
P.xts <- P.xts[,1]


## Yearly average LAI
LAI.spl <- data.frame(dat=LAI[,1],LAI=oLAI.spline$y)
LAI.ordered <- LAI.spl[order(substr(LAI.spl[,1],6,10)),]
LAI.aggr <- aggregate(LAI.ordered[,2], list(monthday=substr(LAI.ordered[,1],6,10)), mean)
LAI.aggr[60,2] <- mean(LAI.aggr[c(59,61),2])
LAI.aggr.xts <- xts(LAI.aggr[,2],as.Date(paste0("2016-",LAI.aggr[,1])))

plot(LAI.aggr.xts, xaxs="i", xlab="",ylab="LAI",main="", major.format="%m")

month.aggr <- as.Date(c(paste0("2016-",1:12,"-01"),"2016-12-31"))

pdf("aggregatedlai.pdf", height=3.5, pointsize=14)
par(mar=c(1.3,2.1,0.6,0.6), las=1, mgp=c(2,1,0))
plot.zoo(LAI.aggr.xts, xaxs="i", yaxs="i", ylim=c(0,7.3), xlab="",ylab="",main="",type="n",xaxt="n")
grid(nx=NA, ny=NULL, lwd=2)
axis(1, at=month.aggr, lab=F, tck=1, col="lightgray", lty="dotted", lwd=2)
lines(as.zoo(LAI.aggr.xts),lwd=3)
axis(1, at=month.aggr, lab=F)
par(mgp=c(1,0.3,0))
## axis.Date(1, at=month.aggr[-13]+15, format="%m", tcl=0)
axis(1, at=month.aggr[-13]+15, lab=month.abb, tcl=0)
box(lwd=2)
dev.off()

## Visualise result
plot(LAI.xts)
lines(LAI.aggr.xts,col=2)
for(tti in 2002:2015)
    lines(xts(LAI.aggr[,2],as.Date(paste(tti,LAI.aggr[,1],sep="-"))),col=2)

## S calculation from original LAI
SAI <- 0.7
## LAI távérzékelésből, SAI állomány magasságból, ha más nincs 0.7
## 0.7 kb. egy 20 méter magas állományra vonatkozik.
## SAI = cs * magasság * densef
## densef= sűrűség (0--1)
## cs = 0.035 vagy (2/allomanymagassag*pi), ha 20 méteres az állomány

Theig <- 20
densef <- 1
cs <- 2/(Theig*pi)
SAI = cs * Theig * densef
SAI <- 0.7 # A little higher then 20

S.int.max <- 0.15* (LAI.xts + SAI) ## Original
S.int.max <- 0.27* (LAI.xts + SAI) ## Misi full LAIval
S.int.max <- 0.29* (LAI.aggr.xts +  SAI) ## Misi aggregated LAIval
LAI.aggr.xts

## Merriam for beech
expression(2.1 * (1 - exp(-(x/2.1))) + 0.1 * x)
curve(2.1 * (1 - exp(-(x/2.1))) + 0.1 * x, 0 , 30)
curve(2.2 * (1 - exp(-(x/2.2))) + 0.1 * x, 0 , 30, add = T, col=2)
curve(1 * (1 - exp(-(x/1))) + 0.1 * x, 0 , 30, add = T, col=4)
curve(0.2 * (1 - exp(-(x/0.2))) + 0.1 * x, 0 , 30, add = T, col=3)

Pakt.xts <- P.xts['2002-07-15/']
Pakt.xts[is.na(Pakt.xts)] <- 0
LAIakt.xts <- LAI.xts['/2014-12-31']

S.int.max <- 0.38*(LAIakt.xts +  SAI)
Esu <-  S.int.max * (1 - exp(-(Pakt.xts/S.int.max))) + 0.1 * Pakt.xts
apply.yearly(Esu,sum)/apply.yearly(Pakt.xts,sum)

## Inter
LAIpacst.xts <- xts(rep(LAI.aggr[-60,2],length(foresee.time)/365),foresee.time)
S.int.max <- 0.38*(LAIpast.xts +  SAI)
Esu <-  S.int.max * (1 - exp(-(Pfre.xts/S.int.max))) + 0.1 * Pfre.xts
past.ratio <- apply.yearly(Esu,sum)/apply.yearly(Pfre.xts,sum)


LAIfutur.xts <- xts(rep(LAI.aggr[-60,2],length(index(Pfre.remo.xts))/365),index(Pfre.remo.xts))
S.int.max <- 0.38*(LAIfutur.xts +  SAI)
Esu.remo <-  S.int.max * (1 - exp(-(Pfre.remo.xts/S.int.max))) + 0.1 * Pfre.remo.xts
remo.ratio <- apply.yearly(Esu.remo,sum)/apply.yearly(Pfre.remo.xts,sum)

grid()

library(ncdf4)
P.nc <- nc_open("fresee2.1_REMO_prec.nc")

## ttfile <- "fresee2.1_RACMO2-ECHAM5_prec.nc"
## ttfile <- "fresee2.1_RCA-HadCM3Q0_prec.nc"
## ttfile <- "fresee2.1_REGCM3-ECHAM5_prec.nc"
## ttfile <- "fresee2.1_HIRHAM5-ECHAM5_prec.nc"
## ttfile <- "fresee2.1_HIRHAM5-ARPEGE_prec.nc"
## ttfile <- "fresee2.1_HadRM3Q0-HadCM3Q0_prec.nc"
## ttfile <- "fresee2.1_CLM-HadCM3Q0_prec.nc"
ttfile <- "fresee2.1_ALADIN-ARPEGE_prec.nc"

P.nc <- nc_open(ttfile)
P.remohv <- ncvar_get(P.nc,"Precipitation", c(3,12,1),c(1,1,31390))
nc_close(P.nc)
akt.time <- seq(as.Date("2015-01-01"),as.Date("2100-12-31"),"days")
foresee.time <- akt.time[-(grep("-02-29",akt.time)+307)]
## Pfre.racmo2echam5.xts <- xts(P.remohv, foresee.time)
Pfre.ALADIN_ARPEGE.xts <- xts(P.remohv, foresee.time)
## Esu.racmo2echam5 <-  S.int.max * (1 - exp(-(Pfre.racmo2echam5.xts/S.int.max))) + 0.1 * Pfre.racmo2echam5.xts
Esu.ALADIN_ARPEGE <-  S.int.max * (1 - exp(-(Pfre.ALADIN_ARPEGE.xts/S.int.max))) + 0.1 * Pfre.ALADIN_ARPEGE.xts
ALADIN_ARPEGE.ratio <- apply.yearly(Esu.ALADIN_ARPEGE,sum)/apply.yearly(Pfre.ALADIN_ARPEGE.xts,sum)


## source("ALADIN_ARPEGE.RData")




all.years <- as.Date(paste0((195:209)*10,"-01-01"))

pdf("intercratio.pdf", width=37, height=3.9, pointsize=14)
par(mar=c(1.5,3.6,0.6,0.6), las=1, mgp=c(2.6,1,0))
plot.zoo(past.ratio,ylim=c(0,0.5), xaxs="i", xlim=as.Date(paste0(c(1951,2100),"-12-31")), typ="n", xaxt="n", ylab="Esu/P ratio")
grid(nx=NA, ny=NULL, lwd=2)
axis(1, at=all.years, lab=F, tck=1, col="lightgray", lty="dotted", lwd=2)
lines(as.zoo(past.ratio),lwd=3)
lines(as.zoo(remo.ratio),lwd=3, col=2)
lines(as.zoo(racmo2echam5.ratio),lwd=3, col=3)
lines(as.zoo(RCA_HadCM3Q0.ratio),lwd=3, col=4)
lines(as.zoo(REGCM3_ECHAM5.ratio),lwd=3, col=5)
lines(as.zoo(HIRHAM5_ECHAM5.ratio),lwd=3, col=6)
lines(as.zoo(ALADIN_ARPEGE.ratio),lwd=3, col=7)
axis(1, at=all.years, lab=F)
par(mgp=c(1,0.5,0))
## axis.Date(1, at=month.aggr[-13]+15, format="%m", tcl=0)
axis.Date(1, at=all.years, tcl=0)
legend("bottomleft", c("Past data","REMO-ECHAM5","RACMO2-ECHAM5","RCA-HadCM3Q0","REGCM3-ECHAM5","REGCM3_ECHAM5","HIRHAM5_ARPEGE","HadRM3Q0-HadCM3Q0","CLM-HadCM3Q0","ALADIN-ARPEGE"), lwd=3, col=c(1,2,3,4,5,6,7,8,9,10), bg="white", ncol=2)
box(lwd=2)
dev.off()
