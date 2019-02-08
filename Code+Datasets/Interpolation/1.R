library(ggplot2)
library(gstat)
library(sp)
library(maptools)

tz <- read.csv("s.csv", header = TRUE)

temp <- tz
temp$x <- temp$location_4
temp$y <- temp$location_3

coordinates(temp) <- ~x + y

plot(temp)
summary(temp$x)
y.range <- as.numeric(c(5017,151745 ))
x.range <- as.numeric(c(5084, 150091))

tz.grid <- expand.grid(x = seq(from = x.range[1],
                               to = x.range[2],
                               by = 250),
                       y = seq(from = y.range[1],
                               to = y.range[2],
                               by = 250))
coordinates(tz.grid) <- ~x + y  
tz.grid@coord
plot(tz.grid, cex = 1.5, col = "grey")
points(temp, pch = 16, col = "blue", cex = 1.5)

idw <- idw(formula = timestamp1 ~ 1,
           locations = temp,
           newdata = tz.grid) 
idw.output <- as.data.frame(idw)
names(idw.output)[1:3] <- c("Longitude", "Latitude", "timestamp1")  

ggplot() + geom_tile(data = idw.output,
                     aes(x = Latitude,
                         y = Longitude,
                         fill = timestamp1)) + 
  geom_point(data = tz, 
             aes(x = location_4, y = location_3),
             shape = 21, 
             color = "red")

tz.contour <- readShapePoly("Export_Output_2.shp")
tz.contour <- fortify(tz.contour)
location_4
ggplot() + geom_tile(data = idw.output, 
                     alpha = 0.8,
                     aes(x = Longitude,
                         y = Latitude,
                         fill = round(timestamp1, 0))) + 
  scale_fill_gradient(low = "cyan", high = "orange") +
  geom_path(data = tz.contour, aes(long, lat, group = group), color = "grey") +
  geom_point(data = tz, aes(x = location_3, y = location_4), shape = 16, cex = 0.5, color = "red") + 
  labs(fill = "Rainfall", title = "10 Year Return Period in Tanzania")

tz.contour <- readShapePoly("Export_Output_2.shp") # use this to clip
tz.contour.dfr <- fortify(tz.contour) # use this to plot

library(raster)
idw.r <- rasterFromXYZ(idw.output[, c("Longitude", "Latitude", "timestamp1")])
idw.crp <- crop(idw.r, tz.contour)
idw.msk <- mask(idw.crp, tz.contour)
idw.msk.dfr <- as.data.frame(rasterToPoints(idw.msk))
names(idw.msk.dfr)[1:2] <- c("Longitude", "Latitude") 
location_4
# now plot:

ggplot() + geom_tile(data = idw.msk.dfr, 
                     alpha = 0.8,
                     aes(x = Longitude,
                         y = Latitude,
                         fill = round(timestamp1, 0))) + 
  scale_fill_gradient(low = "green", high = "red") +
  geom_path(data = tz.contour.dfr, aes(long, lat, group = group), color = "grey") +
  # geom_point(data = tz, aes(x = location_4, y = location_3), shape = 16, cex = 0, color = "white") + 
  labs(fill = "PM 2.5", title = "") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())



# krige
##########################################################################
pr.v<-variogram(timestamp1~1,tz.contour)

pr.vf.exp<-fit.variogram(pr.v,vgm("Exp"))
idw.output<-krige(timestamp1~1,tz.contour,tz.grid,pr.vf.exp)


library(gstat)
gs <- gstat(formula=timestamp1~1, locations=temp)
v <- variogram(gs, width=10)
head(v)
plot(v)
fve <- fit.variogram(v, vgm(100, "Exp", 1500, 20))
plot(fve)
##   model    psill    range
## 1   Nug 21.96600  0.00000
## 2   Exp 85.52957 72.31404
plot(variogramLine(fve, 500000), type='l',ylim=c(0,12))
points(v[,2:3], pch=20, col='red')
fvs <- fit.variogram(v, vgm(85, "Sph", 75, 20))
fvs
##   model    psill    range
## 1   Nug 25.57019   0.0000
## 2   Sph 72.65881 135.7744
plot(variogramLine(fvs, 400), type='l', ylim=c(0,120) ,col='blue', lwd=2)
points(v[,2:3], pch=20, col='red')







