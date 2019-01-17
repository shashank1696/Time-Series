data("AirPassengers")
search()
ap = AirPassengers

names(ap)
table(ap)
ts(100:200, frequency = 7,start = c(12,2) ,calender = TRUE)
print(ts)
ap
ap = window(ap,start = c(1953,1), end = c(1960,12))
summary(ap)
frequency(ap)
cycle(ap)
plot(ap)
aggregate(ap)
boxplot(ap)

unemployment = read.csv("http://rci.rutgers.edu/~rwomack/UNRATE.csv",row.names = 1)
unemployment

names(unemployment)
table(unemployment)

urate = ts(unemployment$VALUE, start = c(1948,1), frequency = 12)
urate

decurate = decompose(urate)
names(decurate)

urate
ap
plot(urate)
window(urate, start = c(1980,7),end = c(1985,7))
acf(urate)
acf(ap)
 
install.packages("fpp")
library(fpp)
library(ggplot2)
library(ggplot)
library(dplyr)
beer
beer2 = window(ausbeer, start = 1992, end = c(2007,4))
beer2

#plot some forecasts
autoplot(beer2)

autolayer(meanf(beer2, h = 11),series = "mean",PI = FALSE)
autolayer(meanf(beer2, h = 11),series = "naive",PI = FALSE)
autolayer(meanf(beer2, h = 11),series = "seasonal naive",PI = FALSE)
ggtitle("Forecasts for quarterly beer production") + 
  xlab("Year") + ylab("Megalitres") +
        guides(colour = guide_legend(title = "Forecast"))
beerfit1 = meanf(beer2,h=11)
plot()

#naive takes immediately previous and means takes all the previous values.
#Exponential smoothing.
#Suitable if the time series has no trend or seasonal component.

data("cafe")
data(cafe)
cafe
class(cafe)
plot(cafe)
es = ses(cafe, h=10)
plot(es)
#shaded area represent 80% & 95% CI

#Model validation
plot(es)
summary(es)

rain = scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip = 1)
rain

names(rain)
table(rain)


rainseries = ts(rain, start(1893))
rainseries_forecasts = HoltWinters(rainseries, beta = FALSE, gamma = FALSE)
rainseries_forecasts
plot(rainseries_forecasts)
#forecast/predict

rainseries_forecasts2 = forecast(rainseries_forecasts,h=8)
plot(rainseries_forecasts2)
#Within expected values
rainseries_forecasts2
checkresiduals(rainseries_forecasts2)
Box.test(rainseries_forecasts2$residuals, lag=20,type = "ljung-Box")

souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir

logsouvenir = log(souvenir)
logsouvenir

souvenirseries = ts(logsouvenir,start = c(1866), frequency = 12)

logsouvenirseries = ts(logsouvenir, start=c(1866),frequency = 12)
logsouvenirseries
plot.ts(logsouvenirseries)

souvenirforecasts = HoltWinters(logsouvenirseries)
plot(souvenirforecasts)

souvenirforecasts2 = forecast(souvenirforecasts,h=48)
souvenirforecasts2

ts.plot(cafe)
fit = ets(cafe)
fcast = forecast(fit, h=12)
fit
?ets
#--------------------------------------------------------------------------------
library(ggplot2)

data("WWWusage")
intusage = WWWusage
class(intusage)
intusage
plot.ts(intusage)
intusagediff1 = diff(intusage,differences = 1)
intusagediff1
plot.ts(intusagediff1)
intusagediff2 = diff(intusage,differences = 2)
plot.ts(intusagediff2)

install.packages("tseries")
library(tseries)
adf.test(intusage)
adf.test(intusagediff1)
adf.test(intusagediff2)

