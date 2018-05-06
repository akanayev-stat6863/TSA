#import csv file
air_m=read.csv("C:\\Users\\Azamat\\Downloads\\monthly-us-air-passenger-miles.csv")
#plot air time series data
plot.ts(air_m$air.passenger.miles)
#make monthly time series data from Jan 1960 to DEc 1977
air_time=ts(data=air_m$air.passenger.miles, frequency=12, start=c(1960, 1))
length(air_time)

length(air_m$air.passenger.miles)

#creating dummy variables from 1 to 11, Dec as reference
dummies=table(1:length(month), as.factor(month))
length(dummies)



length(air_time)
month = rep(1:12,36)

month

#fit time series regression
fit = lm(air_time~time(air_time)+dummies[,1:11])

fit3= lm(air_time~time(air_time) + factor(month))

month  = rep(1:12, 18)
dummies = table(1:length(month), as.factor(month))
mod=(lm(air_time~0+time(air_time) + dummies[,1:11]))
summary(mod)
acf(mod$res)
pacf(mod$res)

mod1=(lm(air_time~time(air_time) + dummies[,c(2,6:8,11)]))
summary(mod1)
acf(resid(mod1))
pacf(resid(mod1))

mod1=lm(air_time~time(air_time))



mod2=(lm(air_time~0+time(air_time) + dummies[,c(2,6:8,11)]))
summary(mod1)
dummies

f=predict(mod2, n.ahead=12)
f

plot(time(air_time), fitted(mod1), col='red', type='l')
lines(air_time)
acf(resid(mod1))
plot(resid(mod1))
plot(resid(mod1), type='l')


pred = c(fitted(fit3), -4004.75+2*c(2011, 2011.25, 2011.5, 2011.75) + c(0, 21, 33.5, 4.5))
plot( c(time(bike), 2011,2011.25, 2011.5,2011.75), pred, type='l')
> points(bike, type='o', col='red')
> pred


pred = c(fitted(mod1), -1770-0.9031)
plot( c(time(air_time), 1978), pred, type='l')
> points(bike, type='o', col='red')
> pred

a=time(air_time)
a
time(bike)

predict(mod1, data.frame(a=12), interval="prediction")
mod1$coefficients
summary(mod1)

air_time

pred = c(fitted(mod1), -1769+0.9023*c(1978.01, 1978.02, 1978.03, 1978.04, 1978.05, 1978.06, 1978.07, 1978.08, 1978.09, 1978.10, 1978.11, 1978.12) + c(0, -0.954, 0, 0, 0, 1.119, 1.449, 1.952, 0, 0, -1.031, 0))
plot( c(time(air_time), 1978.000, 1978.083, 1978.167, 1978.250, 1978.333, 1978.417, 1978.500, 1978.583, 1978.667, 1978.750, 1978.833, 1978.917), pred, type='l')
points(air_time, type='o', col='red')
pred




pred = c(fitted(mod1), data.frame(-1769+0.9023*c(1978.01, 1978.02, 1978.03, 1978.04, 1978.05, 1978.06, 1978.07, 1978.08, 1978.09, 1978.10, 1978.11, 1978.12) + c(0, -0.954, 0, 0, 0, 1.119, 1.449, 1.952, 0, 0, -1.031, 0)), interval="prediction")
pred

summary(mod1)
mod1$coefficients
predict(mod1, c(14.81, 16.92, 17.26, 17.77, 14.82), interval="prediction")

summary(mod1)        
        
        

log(jj)= ??*time + ??1*Q1 + ??2*Q2 + ??3*Q3 + ??4*Q4 + ??

library(astsa)
Q      = factor(cycle(jj))        # make (Q)uarter factors
trend  = time(jj) - 1970          # not necessary to "center" time, but the results look nicer
reg    = lm(log(jj)~ 0 + trend + Q, na.action=NULL)  # run the regression without an intercept
#-- the na.action statement is to retain time series attributes
summary(reg)

jj
M = factor(cycle(air_time))
M
trend1 = time(air_time)-1969
reg1    = lm(log(air_time)~ 0 + trend1 + M, na.action=NULL)
summary(reg1)
model.matrix(reg1) #trend is time centered at 1969

plot(log(air_time), type="o")    # the data in black with little dots 
lines(fitted(reg1), col=2)  # the fitted values in bloody red 

which shows that a plot of the data with the fit superimposed is not fit well

par( mfrow = c(2,1) )
plot( resid(reg1) )       # residuals
acf( resid(reg1), 20 )    # acf of the resids 
pacf( resid(reg1), 20 )    # pacf of the resids 

acf(diff(log(air_time)))

plot(dog <- stl(log(air_time), "per")) 

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
summary(mod1)
plot(mod1)
plot(reg1)
reg1$coefficients
predict(reg1, data.frame(trend1=1, M1=1), interval="predict")



jj
time(jj)
air_time


milk=read.csv("C:\\Users\\Azamat\\Downloads\\milk.csv")
length(milk$Prod)

