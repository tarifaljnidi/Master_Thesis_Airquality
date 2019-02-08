library(sp)
library(gstat)
x <- read.csv("s.csv")
x$OZDLYAV <- x$pm2.5 * 1000
library(sp)
coordinates(x) <- ~LONGITUDE + LATITUDE
proj4string(x) <- CRS('+proj=longlat +datum=NAD83')
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +ellps=GRS80")

library(gstat)
gs <- gstat(formula=pm2.5~1, locations=x)
v <- variogram(gs, width=20)
head(v)
plot(v)
fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))
plot( cex.lab=1.5,yaxt='n',xaxt = 'n',variogramLine(fve, 400), type='l', col='blue',ylim=c(0,120),ylab="Semivariance",xlab="Distance (Km)")
axis(1,at=c(0,100,200,300,400), labels=c("0","20","40","60","80"))
axis(2,at=c(0,20,40,60,80,100), labels=c("0","10","20","30","40","50"))
points(v[,2:3], pch=20,cex=1.2, col='red')
head(fve)

fvs <- fit.variogram(v, vgm(85, "Sph", 750, 20))
plot( cex.lab=1.5,yaxt='n',xaxt = 'n',variogramLine(fvs, 400), type='l', ylim=c(0,120) ,col='blue', ylab="Semivariance",xlab="Distance (Km)")
axis(1,at=c(0,100,200,300,400), labels=c("0","20","40","60","80"))
axis(2,at=c(0,20,40,60,80,100), labels=c("0","10","20","30","40","50"))
points(v[,2:3], pch=20, cex=1.2,col='red')
head(fvs)

fvg <- fit.variogram(v, vgm(85, "Gau", 75, 20))
plot( cex.lab=1.5,yaxt='n',xaxt = 'n',variogramLine(fvg, 400), type='l', ylim=c(0,120) ,col='blue', ylab="Semivariance",xlab="Distance (Km)")
axis(1,at=c(0,100,200,300,400), labels=c("0","20","40","60","80"))
axis(2,at=c(0,20,40,60,80,100), labels=c("0","10","20","30","40","50"))
points(v[,2:3], pch=20, cex=1.2,col='red')
head(fvg)
library(gstat)

fvm <- fit.variogram(v, vgm(85, "Mat", 75, 20))
plot(variogramLine(fvm, 400), type='l', ylim=c(0,120) ,col='blue', ylab="Semivariance",xlab="Distance (Km)",lwd=2)
points(v[,2:3], pch=20, col='red')
head(fvm)
#goodness of fit
#d value,
attr(fvm, 'SSErr')
attr(fvs, 'SSErr')
attr(fvg, 'SSErr')
attr(fve, 'SSErr')

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
library(dismo)
set.seed(5132015)
kf <- kfold(nrow(x))

k<-NULL
rmse <- rep(NA, 5)
for (k in 1:5) {
  test <- x[kf == k, ]
  train <- x[kf != k, ]
  gs <- gstat(formula=pm2.5~1, locations=train)
  v <- variogram(gs, width=20)
  fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))
  p <- predict(fve, test)
  rmse[k] <- RMSE(test$prec, p$var1.pred)
}
rmse
Gau
Mat
Tab
