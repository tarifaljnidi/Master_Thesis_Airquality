library(forecast)
library(fpp)
library(ggplot2)
oildata <- window(oil, start=1996)
# Estimate parameters
fc <- ses(oildata, h=5)
head(fc)

mydata <-read.table(file="meandataset2.csv",header=T,sep=",",na.strings =" ")
attach(mydata)

dat <- data.frame(myts = mydata$pm2.5,
                  Date = seq(as.Date("2018-11-07"), as.Date("2018-11-07") + 46, by = 1))
str(dat)
startW <- as.numeric(strftime(head(dat$Date, 1), format = "%W"))
startD <- as.numeric(strftime(head(dat$Date, 1) + 1, format =" %w")) 
t<-ts(dat$myts, frequency = 7, start = c(startW, startD))

ID <- 45:52
autoplot(t)+
  ggtitle("") +
  ylab("PM2.5 Value (µg/m3)") +
  xlab("Lags (weeks)")+
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
   theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=14)
  )
  

ggseasonplot(t,polar=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")
ggsubseriesplot(t) +
  ylab("PM2.5 Value (µg/m3)") +
  ggtitle("")+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=12)
  )

gglagplot(t)
g<-ggAcf(t,lag=80)
g+xlab("Lags (days)")+ ggtitle("Autocorrelation of the time series data")+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=12)
  )
###################################################
smpl2 <- window(t, start = c(50, 1), end = c(52,1))
fit <- nnetar(window(t,end=c(50, 1)))
sim <- ts(matrix(0, nrow=3, ncol=5),start = 50)
for(i in seq(5))
  sim[,i] <- simulate(fit, nsim=3)

library(ggplot2)
autoplot(t) + forecast::autolayer(sim)+
  ggtitle("") +
  ylab("PM2.5 Value (µg/m3)") +
  xlab("Lags (weeks)")+
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )


fcast <- forecast(fit, PI=TRUE, h=14)
autoplot(PI=FALSE, fcast,ylab="PM2.5 Value (µg/m3)", xlab="Lags (weeks)")+
  
  autolayer(smpl2, series="Data",color="black") +
  autolayer(fcast$mean, series="Forecasts",color="blue")+
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )+
  ggtitle("")
###################################################
smpl2 <- window(t, start = c(50, 1), end = c(52,1))
#set.seed(12345)
fit <- nnetar(window(t,end=c(50, 1)))
library(ggplot2)
fcast <- forecast(fit ,PI=TRUE,h=14)
autoplot( PI=TRUE,fcast,ylab="PM2.5 Value (µg/m3)", xlab="Lags (weeks)")+

  autolayer(smpl2, series="Data",color="black") +
  autolayer(fcast$mean, series="Forecasts",color="blue")+
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )+
  ggtitle("")
#accuracy nnar
###############

#autoplot(fcast)
round(accuracy(fcast),3)

res <- residuals(fit)
autoplot(res) + xlab("Lags (weeks)") + ylab("PM 2.5 value (µg/m3)") +
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )
  ggtitle("")
ggAcf(res,lag=29) + ggtitle("")+ xlab("Lags (days)")
gghistogram(res) + ggtitle("Histogram of residuals")

checkresiduals(fit,ylab="PM 2.5 value (µg/m3)")


cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("lags( weeks)") + ylab("PM 2.5 value (µg/m3)")
y1<-na.omit(fit$residuals)
y2<-na.omit(fcast$residuals)
mean(fit$residuals)
mean(fcast$residuals)
mean(y1)
mean(y2)
###################################################

#modification nnar
set.seed(12345)
fitnnarimproved <- nnetar(window(t,end=c(50, 1)))
fit_cv <- CVar(t)
res_sd <- sd(fit_cv$residuals, na.rm=TRUE)
myinnovs <- rnorm(14*100, mean=0, sd=res_sd)
## Forecast using new innovations
NNAR <- forecast(fitnnarimproved,PI=TRUE, h=14, npaths=100, innov=myinnovs)
autoplot(PI=FALSE,NNAR,ylab="PM2.5 Value (µg/m3)",xlab="Lags (weeks)")+

  autolayer(smpl2, series="Data",color="black")+ 
autolayer(NNAR$mean, series="Forecasts", color="blue")+
  
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )+
  ggtitle("")
lambda <- BoxCox.lambda(t)

round(accuracy(NNAR),3)
y1<-na.omit(fitnnarimproved$residuals)
mean(y1)
y2<-na.omit(NNAR$residuals)
mean(y2)
res <- residuals(fitnnarimproved)
autoplot(res) + xlab("Lags (weeks)") + ylab("PM 2.5 value (µg/m3)") +
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )
ggtitle("")
ggAcf(res,lag=33) + ggtitle("")+ xlab("Lags (days)")
gghistogram(res) + ggtitle("Histogram of residuals")


###################################################
###################################################
fitets <- ets(window(t,end=c(50, 1)))
summary(fitets)
autoplot(fitets)+ggtitle("components of ETS method") 
ETS <- forecast(fitets, PI=TRUE, h=14)

autoplot(PI=TRUE,ETS,ylab="PM2.5 Value (µg/m3)",xlab="Lags (weeks)")+
  
  autolayer(smpl2, series="Data",color="black")+ 
  autolayer(ETS$mean, series="Forecasts", color="blue")+
  
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )+ ggtitle("")

#accuracy ets
###############

autoplot(ETS)
round(accuracy(ETS),3)

res <- residuals(fitets)
autoplot(res) + xlab("Lags (weeks)") + ylab("PM 2.5 value (µg/m3)") +
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )
ggtitle("")
ggAcf(res,lag=80) + ggtitle("")+ xlab("Lags (days)")
gghistogram(res) + ggtitle("Histogram of residuals")

checkresiduals(fitets,ylab="PM 2.5 value (µg/m3)")+ggtitle("")


cbind('Residuals' = residuals(fitets),
      'Forecast errors' = residuals(fitets,type='response')) %>%
  autoplot(facet=TRUE) + xlab("lags( weeks)") + ylab("PM 2.5 value (??g/m3)")

y1<-na.omit(fitets$residuals)
mean(y1)


###########################################################################


Combination <- (ETS[["mean"]] + NNAR[["mean"]] )/2

autoplot(t) +
  autolayer(ETS, series="ETS", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  autolayer(Combination, series="Combination") +
  xlab("Lags (weeks)") + ylab("PM2.5 Value (µg/m3)")  +
  scale_x_continuous( labels = as.character(ID), breaks = ID)+
  theme(
    plot.title = element_text(size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue",size=14, face="bold"),
    axis.title.y = element_text(color="blue",size=14, face="bold"),
    axis.text=element_text(size=13),
    legend.text=element_text(size=13)
  )
ggtitle("")
round(accuracy(Combination),3) 

  c(ETS = accuracy(ETS, t)["Test set","MASE"],
    NNAR = accuracy(NNAR, t)["Test set","MASE"],
    Combination =
      accuracy(Combination, t)["Test set","MASE"])
  
  
  