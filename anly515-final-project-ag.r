#this is new working directory
getwd()
setwd("/Users/ameyaghag/Documents/Harrisburg University/ANLY 515/Final Project")

#installing and loading necessary packages
install.packages("QRM")
library(QRM)
library(readxl)
library(timeSeries)
library(FRAPO)
library(fGarch)
library(readr)

#reading data from source of both data sets
#old tech data set contains stock prices of msft,adobe and salesforce over last 5 years
#new tech data set contains stock prices of fb, tesla and netflix over last 5 years

library(readxl)
old_tech <- read_excel("old_tech.xls", col_types = c("date", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))
View(old_tech)

new_tech <- read_excel("new_tech.xls", col_types = c("date", 
                                                     "numeric", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric"))
View(new_tech)

#removing rows with any NA data using na.omit

old_tech<-na.omit(old_tech)
new_tech<-na.omit(new_tech)

#storing date of stock prices in separate variable, for remerging later, or use in timeseries function

date_old<-old_tech$Date
date_new<-new_tech$Date

str(date_old)
str(date_new)

#removing daily volume field from each portfolio

old_tech2<-cbind(old_tech$Date, old_tech$msft_price, old_tech$adbe_price, old_tech$crm_price)
colnames(old_tech2)<-c("Date","msft_price","adbe_price","crm_price")


new_tech2<-cbind(new_tech$Date, new_tech$fb_price, new_tech$nflx_price, new_tech$tsla_price)
colnames(new_tech2)<-c("Date","fb_price","nflx_price","tsla_price")

#creating timeseries of each portfolio here, using date stored above as charvec

oldTS<-timeSeries(old_tech2[,-1], charvec = date_old)
newTS<-timeSeries(new_tech2[,-1], charvec = date_new)

head(oldTS)
head(newTS)

#plotting timeseries of both old and new portfolios

plot(oldTS)
plot(newTS)

#Calculating losses of each portfolio and save the results as one data frame.

oldTechLoss<-as.data.frame(na.omit(-1.0*diff(log(oldTS))*100.0))
colnames(oldTechLoss)<-c("msft","adbe","crm")


newTechLoss<-as.data.frame(na.omit(-1.0*diff(log(newTS))*100.0))
colnames(newTechLoss)<-c("fb","nflx","tsla")

head(oldTechLoss)
head(newTechLoss)

#Plot ACF and PACF functions for each of the assets, and determine the rank of the ARIMA model.
#using par(mfrow) to make sure all 6 plots are visible in one place 

par(mfrow = c(3,2))

acf(oldTechLoss$msft,na.action = na.pass)
pacf(oldTechLoss$msft,na.action = na.pass)

acf(oldTechLoss$adbe,na.action = na.pass)
pacf(oldTechLoss$adbe,na.action = na.pass)

acf(oldTechLoss$crm,na.action = na.pass)
pacf(oldTechLoss$crm,na.action = na.pass)

par(mfrow = c(3,2))

acf(newTechLoss$fb,na.action = na.pass)
pacf(newTechLoss$fb,na.action = na.pass)

acf(newTechLoss$nflx,na.action = na.pass)
pacf(newTechLoss$nflx,na.action = na.pass)

acf(newTechLoss$tsla,na.action = na.pass)
pacf(newTechLoss$tsla,na.action = na.pass)

mean(oldTechLoss$msft,na.rm=TRUE)
mean(oldTechLoss$adbe,na.rm=TRUE)
mean(oldTechLoss$crm,na.rm=TRUE)

mean(newTechLoss$fb,na.rm=TRUE)
mean(newTechLoss$nflx,na.rm=TRUE)
mean(newTechLoss$tsla,na.rm=TRUE)

install.packages("forecast")
library(forecast)

#use auto arima from package forecast to look at rank of arima model

arimaMSFT<-auto.arima(oldTechLoss$msft)
arimaADBE<-auto.arima(oldTechLoss$adbe)
arimaCRM<-auto.arima(oldTechLoss$crm)

arimaFB<-auto.arima(newTechLoss$fb)
arimaNFLX<-auto.arima(newTechLoss$nflx)
arimaTSLA<-auto.arima(newTechLoss$tsla)

#using auto arima and predicting losses for 2 ahead looking periods

class(arimaMSFT)
predict(arimaMSFT, n.ahead=2)

class(arimaADBE)
predict(arimaADBE, n.ahead=2)

class(arimaCRM)
predict(arimaCRM, n.ahead=2)


class(arimaFB)
predict(arimaFB, n.ahead=2)

class(arimaNFLX)
predict(arimaNFLX, n.ahead=2)

class(arimaTSLA)
predict(arimaTSLA, n.ahead=2)

#Using ARMA 00 and Garch 11 to predict losses one period ahead

gMSFT<-garchFit(~arma(0,0)+garch(1,1), data=oldTechLoss$msft)
gADBE<-garchFit(~arma(0,0)+garch(1,1), data=oldTechLoss$adbe)
gCRM<-garchFit(~arma(0,0)+garch(1,1), data=oldTechLoss$crm)

gFB<-garchFit(~arma(0,0)+garch(1,1), data=newTechLoss$fb)
gNFLX<-garchFit(~arma(0,0)+garch(1,1), data=newTechLoss$nflx)
gTSLA<-garchFit(~arma(0,0)+garch(1,1), data=newTechLoss$tsla)


predict(gMSFT, n.ahead = 1)
predict(gADBE, n.ahead = 1)
predict(gCRM, n.ahead = 1)

predict(gFB, n.ahead = 1)
predict(gNFLX, n.ahead = 1)
predict(gTSLA, n.ahead = 1)

#defining function for calculating ES using garch at 95% confidence

ESgarch <- function(y, p = 0.95){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  as.numeric(predict(gfit, n.ahead = 1)[3])
  df <- as.numeric(coef(gfit)["shape"])
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) *
    ((df + (qt(p, df))^2)/(df - 1))
  return(ES)
}

msftES<-ESgarch(na.omit(oldTechLoss$msft))
msftES

adbeES<-ESgarch(na.omit(oldTechLoss$adbe))
adbeES

crmES<-ESgarch(na.omit(oldTechLoss$crm))
crmES

fbES<-ESgarch(na.omit(newTechLoss$fb))
fbES

nflxES<-ESgarch(na.omit(newTechLoss$nflx))
nflxES

tslaES<-ESgarch(na.omit(newTechLoss$tsla))
tslaES

#calculating returns of all stocks

msftReturns<-returnseries(old_tech$msft_price)

adbeReturns<-returnseries(old_tech$adbe_price)

crmReturns<-returnseries(old_tech$crm_price)

oldTechReturns<-cbind(msftReturns,adbeReturns,crmReturns)
head(oldTechReturns)

fbReturns<-returnseries(new_tech$fb_price)

nflxReturns<-returnseries(new_tech$nflx_price)

tslaReturns<-returnseries(new_tech$tsla_price)

newTechReturns<-cbind(fbReturns,nflxReturns,tslaReturns)
head(newTechReturns)

#covariance matrix by using cov() function and use use="pairwise.complete.obs" specification

library(FRAPO)
oldTechCov<-cov(oldTechReturns, use="pairwise.complete.obs")
oldTechCov



library(FRAPO)
oldGMV<-PGMV(oldTechCov)
oldGMVw<-Weights(oldGMV)

wMSFT<- as.numeric(oldGMVw[1])/100
wADBE<- as.numeric(oldGMVw[2])/100
wCRM<- as.numeric(oldGMVw[3])/100

newTechCov<-cov(newTechReturns, use="pairwise.complete.obs")
newTechCov

newGMV<-PGMV(newTechCov)
newGMVw<-Weights(newGMV)

wFB<- as.numeric(newGMVw[1])/100
wNFLX<- as.numeric(newGMVw[2])/100
wTSLA<- as.numeric(newGMVw[3])/100

MSFTreturn<-(-1)*oldTechLoss$msft
ADBEreturn<-(-1)*oldTechLoss$adbe
CRMreturn<-(-1)*oldTechLoss$crm


portretOld<-MSFTreturn*wMSFT+ADBEreturn*wADBE+CRMreturn*wCRM
mean(portretOld)

oldTechVariance<-ESgarch(portretOld)
oldTechVariance


FBreturn<-(-1)*newTechLoss$fb
NFLXreturn<-(-1)*newTechLoss$nflx
TSLAreturn<-(-1)*newTechLoss$tsla

#expected monthly return of the GMVP portfolio? 

portretNew<-FBreturn*wFB+NFLXreturn*wNFLX+TSLAreturn*wTSLA
mean(portretNew)

newTechVariance<-ESgarch(portretNew)
newTechVariance



#for Old Tech

#data frame to estimate garch model for each of the assets

gfitOLD<-lapply(oldTechLoss,garchFit,formula=~arma(0,0)+garch(1,1),cond.dist="std",trace=FALSE)
gfitOLD

#varible to generate one-step-ahead forecasts of the conditional variance (Standard Deviation)

gprog<-unlist(lapply(gfitOLD,function(x) predict(x,n.ahead = 1)[3]))

#Estimate degrees-of-freedom parameters for the garch model of each asset. 

gshape<-unlist(lapply(gfitOLD, function(x) x@fit$coef[5]))

#conditional standardized residuals for each asset as save as a matrix.

gresid<-as.matrix(data.frame(lapply(gfitOLD,function(x) x@residuals / sqrt(x@h.t))))
head(gresid)

U <- sapply(1:3, function(y) pt(gresid[, y], df = gshape[y]))
head(U)
hist(U)

library(timeSeries)
library(FRAPO)
library(fGarch)


library(QRM)

#Estimate Student's t copula model based on Kendall's rank correlations. 

cop <- fit.tcopula(Udata = U, method = "Kendall")

#Use the dependence structure determined by the estimated copula 
#     for generating 100,000 data sets of random variates for the pseudo-uniformly 
#     distributed variables. 

rcop <- rcopula.t(100000, df = cop$nu, Sigma = cop$P)
head(rcop)
hist(rcop)

qcop <- sapply(1:3, function(x) qstd(rcop[, x], nu = gshape[x]))
head(qcop)
hist(qcop)

#Create a matix of 1 period ahead predictions of standard deviations. 

ht.mat <- matrix(gprog, nrow = 100000, ncol = ncol(oldTechLoss), byrow = TRUE)
head(ht.mat)

weights<-c(wMSFT,wADBE,wCRM)



pfall <- (qcop * ht.mat) %*% weights


OLDpfall.es95 <- median(tail(sort(pfall), 5000))
OLDpfall.es95 #1.74
OLDpfall.var95 <- min(tail(sort(pfall), 5000))
OLDpfall.var95 #1.30

-----------------------------------------------------
  
#repeating procedure for NEW Tech
  
gfitNEW<-lapply(newTechLoss,garchFit,formula=~arma(0,0)+garch(1,1),cond.dist="std",trace=FALSE)
gfitNEW

gprog<-unlist(lapply(gfitNEW,function(x) predict(x,n.ahead = 1)[3]))

gshape<-unlist(lapply(gfitNEW, function(x) x@fit$coef[5]))

gresid<-as.matrix(data.frame(lapply(gfitNEW,function(x) x@residuals / sqrt(x@h.t))))
head(gresid)

U <- sapply(1:3, function(y) pt(gresid[, y], df = gshape[y]))
head(U)
hist(U)

cop <- fit.tcopula(Udata = U, method = "Kendall")

rcop <- rcopula.t(100000, df = cop$nu, Sigma = cop$P)
head(rcop)
hist(rcop)

qcop <- sapply(1:3, function(x) qstd(rcop[, x], nu = gshape[x]))
head(qcop)
hist(qcop)


ht.mat <- matrix(gprog, nrow = 100000, ncol = ncol(newTechLoss), byrow = TRUE)
head(ht.mat)

weights<-c(wFB,wNFLX,wTSLA)

pfall <- (qcop * ht.mat) %*% weights


NEWpfall.es95 <- median(tail(sort(pfall), 5000))
NEWpfall.es95 #2.9
NEWpfall.var95 <- min(tail(sort(pfall), 5000))
NEWpfall.var95 #2.12
