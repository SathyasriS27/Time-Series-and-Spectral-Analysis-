#Clearning the Environment 
rm(list=ls())
#Importing the Libraries 
library(TSA)
library(forecast)
#Loading the data 
load("slavery.RData")
xf
class(xf)
par(mfrow=c(1,1))
####################################################################
####################################################################
#TASK 1 
#plotting the data - xf
plot(xf$year, xf$num, pch=20, xlab="Year", ylab="Number of Slaves")
lines(xf$year,xf$num,col="blue",lty="dotted")
lines(xf$year,trend, col="red")
#linear regression -  plot in the main graph
xf.lm<-lm(formula=xf$num~xf$year, data=xf)
xf$values<-xf.lm$fitted.values
lines(xf$year, xf$values, col="black")
#Finding the residuals 
n=length(xf$num)
t1=as.numeric(time(xf$num))
trend=lm(xf$num~t1)
Y=residuals(trend)
plot(t1,Y,pch=20,xlab="",ylab="")
lines(t1,Y,col="blue",lty="dotted")
title("Residuals - Y")
##################################################################
#No seasonality present 
xf1 <- ts(xf, frequency = frequency(xf$year), start = start(xf$year),end=end(xf$year))
xf1tscheck <- decompose(xf1)#R code to directly check if seasonality exists 
#################################################################
#################################################################
#Task 2 
par(mfrow=c(2,1))
acf(Y,lag=40)
pacf(Y)
#It is an AR process of order 1 - according to the ACF and PACF plots
################################################################
################################################################
#Task 3 
ar(Y,aic=FALSE,order.max=1,method = c("yule-walker"))
ar(Y,aic=FALSE,order.max=2,method = c("yule-walker"))
ar(Y,aic=FALSE,order.max=3,method = c("yule-walker"))

###############################################################
###############################################################
#Task 4 
ar1=ar(Y,aic=FALSE,order.max=1)$resid
ar2=ar(Y,aic=FALSE,order.max=2)$resid
ar3=ar(Y,aic=FALSE,order.max=3)$resid

#replacing nan with 0 
ar1[is.na(ar1)]<-0
ar2[is.na(ar2)]<-0
ar3[is.na(ar3)]<-0
#for res^2
ar1_2=ar(Y,aic=FALSE,order.max=1)$resid^2
ar2_2=ar(Y,aic=FALSE,order.max=2)$resid^2
ar3_2=ar(Y,aic=FALSE,order.max=3)$resid^2
#replacing nan with 0 
ar1_2[is.na(ar1_2)]<-0
ar2_2[is.na(ar2_2)]<-0
ar3_2[is.na(ar3_2)]<-0
#Plotting the ar1 plots 
par(mfrow=c(3,3))
#AR1 plot
plot(ar1,type='l')
a1<-acf(ar1,plot=FALSE)
plot(a1, main="ACF of AR(1) residuals")
a11<-acf(ar1_2,plot=FALSE)
plot(a11, main="ACF of AR(1) squared residuals ")
#AR2 plot 
plot(ar2,type='l')
a2<-acf(ar2,plot=FALSE)
plot(a2, main="ACF of AR(2) residuals")
a22<-acf(ar2_2,plot=FALSE)
plot(a22, main="ACF of AR(2) squared residuals ")
#AR3 plot
plot(ar3,type='l')
a3<-acf(ar3,plot=FALSE)
plot(a3, main="ACF of AR(3) residuals")
a33<-acf(ar3_2,plot=FALSE)
plot(a33, main="ACF of AR(3) squared residuals ")
#Residuals are useful in checking whether a model has adequately 
#captured the information in the data. 
#A good forecasting method will yield residuals with the 
#following properties:
#The residuals are uncorrelated. 
#If there are correlations between residuals,
#then there is information left in the residuals which 
#should be used in computing forecasts.
#The residuals have zero mean.
#If the residuals have a mean other than zero, 
#then the forecasts are biased.
Z=ar1
#################################################################
#################################################################
#Task 5 
X=xf$num
par(mfrow=c(2,3))
periodogram(X,log="no")
periodogram(Y,log="no")
periodogram(Z,log="no")
spec.pgram(X,log="no")
spec.pgram(Y,log="no")
spec.pgram(Z,log="no")
#################################################################
#################################################################
#Task 6 
par(mfrow=c(1,1))
ar_arima=arima(Y,order=c(2,0,0))
print(ar_arima)
#################################################################
#################################################################
#Task 7 
fit <- xf$num-(10^ar2)
ar1
fit
plot(xf$year,xf$num,type='l')
lines(xf$year,fit,type="l",col=2,lty=2)
final_ans=ar(fit,order=2,aic=FALSE)
predict(final_ans, n.ahead=5)
##################################################################
#################################################################
auto.arima(X)
auto.arima(Y)
auto.arima(Z)

