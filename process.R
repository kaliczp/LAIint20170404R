LAI <- read.csv("LAIday.csv")
library(xts)
LAI.xts <- xts(LAI[,2], as.Date(LAI[,1]))
