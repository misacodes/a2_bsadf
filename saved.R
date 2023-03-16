#Housekeeping
rm(list = ls()) 

#Set Seed
set.seed(2346)

#installing packages
# install.packages("xlsx")
# install.packages("MultipleBubbles")
# install.packages("moments")
# install.packages("corrgram")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("rtadfr")
# install.packages("psymonitor")
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("lubridate")
# install.packages("markdown")
# install.packages("spelling")
# install.packages("testthat")
# install.packages("PerformanceAnalytics")
# install.packages("xts")
# install.packages("expm")
install.packages('aTSA')

#Loading packages
library("xlsx")
library("MultipleBubbles")
library("moments")
library("corrgram")
library("forecast")
library("tseries")
library("remotes")
library("psymonitor")
library("ggplot2")
library("knitr")
library("lubridate")
library("markdown")
library("spelling")
library("testthat")
library("PerformanceAnalytics")
library("xts")
library("expm")
library('aTSA')

#Loading and Defining data
ethereumdata <- read_excel("Desktop/ethereumdata.xlsx")
View(ethereumdata)
P = ethereumdata$Open
IQR(P)
logP = ethereumdata$`ln open`
date = ethereumdata$Date
date
acf(P, lag.max = 20,main='ACF for ETH Price Series')
pacf(P, lag.max = 20, main='Partial ACF for ETH Price Series')
arch.test(logP, output=TRUE)
plot(P, xaxt ="n", type='l')
axis(1, at = date, labels = FALSE)
plot(date,P,type="l")
axis.Date(1,at=date,las=2)
pt + scale_x_date(breaks = as.Date(c("2017-01-01","2018-01-01")))
#Returns for homoskedasticity
P_xts <- xts(P, order.by = date) 
Return <- Return.calculate(P_xts, method = "discrete")
plot.ts(Return)
Return2 <- Return^2
plot.ts(Return2)
acf(Return2, lag.max = 100, plot = TRUE, main = "ACF for log Prices", ylab = "Autocorrelation", type = c("correlation", "covariance"))             
McLeod.                                                                                                                                                                                                    "partial"))

#Describing Data
skewness(logP)
kurtosis(logP)
summary(logP)
sd(logP)

acf(logP, lag.max = 100, main = "ACF for log Prices", ylab = "Autocorrelation", type = c("correlation", "covariance",
                                "partial"))
pacf(logP, lag.max = 100)
hist(logP)

#Calculate the Dickey Fuller with a Fixed Lag Order
#ADF without a constant
#Can't calculate lags without a constant
adf.test(logP, alternative = c("explosive"))

ADF_FL(logP, adflag = 0, mflag = 3)

#ADF with constant 
ADF_FL(logP, adflag = 0, mflag = 1)
ADF_FL(logP, adflag = 1, mflag = 1)
ADF_FL(logP, adflag = 2, mflag = 1)

#ADF with trend
ADF_FL(logP, adflag = 0, mflag = 2)
ADF_FL(logP, adflag = 1, mflag = 2)
ADF_FL(logP, adflag = 2, mflag = 2)

#Calculate the ADF which considers AIC and BIC
#constant no trend
ADF_IC(logP, adflag = 20, mflag = 1, IC = 1)
ADF_IC(logP, adflag = 20, mflag = 1, IC = 2)

#constant trend
ADF_IC(logP, adflag = 20, mflag = 2, IC = 1)
ADF_IC(logP, adflag = 20, mflag = 2, IC = 2)

#Non-recursive ADF
adf.test(logP, alternative = "explosive" , k=1)

#Defining vars
obs <- length(logP)
swindow0 <- floor(obs*(0.01 + 1.8/sqrt(obs)))
dim <- obs - swindow0 + 1
Tb <- 24 + swindow0 - 1

# Estimate PSY statistics and CVs
# bsadf <- PSY(logP, swindow0)
bsadf <- readRDS('bsadf.rds')
quantilesBsadf <- cvPSYwmboot(logP, swindow0, Tb=Tb, nboot = 49, nCores = 2)
quantiles <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)

quantiles90 <- quantiles[1,]
quantiles95 <- quantiles[2,]
quantiles99 <- quantiles[3,]

# locate bubble/crisis dates, CI99
ind99 <- (bsadf > t(quantiles99)) * 1
monitorDates <- date[swindow0:obs]
OT <- locate(ind99, monitorDates)
OT

# locate bubble/crisis dates, CI95
ind95 <- (bsadf > t(quantiles95)) * 1
monitorDates <- date[swindow0:obs]
OT <- locate(ind95, monitorDates)

# locate bubble/crisis dates, CI90
ind90 <- (bsadf > t(quantiles90)) * 1
monitorDates <- date[swindow0:obs]
OT <- locate(ind90, monitorDates)

# Show bubble/crisis periods
print(OT, obs)
disp(OT, obs)

#Locate bubble/crisis periods
date[ind99==1]
dateStampDf1 <- ts(bsadf)
dateStampDf2 <- ts(quantiles90)
dateStampDf3 <- ts(quantiles95)
dateStampDf4 <- ts(quantiles99)
ts.plot(dateStampDf1, plot.type = "single", col="blue")
ts.plot(dateStampDf2, plot.type = "single", col="red")
ts.plot(dateStampDf3, plot.type = "single", col="#005100")
ts.plot(dateStampDf4, plot.type = "single", col="purple")


indbubble <- (dateStampDf1 > dateStampDf4) * 1
bubble_line <- indbubble - 6

dateStampDf <- cbind(dateStampDf1, dateStampDf4, bubble_line) 
write.csv(dateStampDf,'301_citation_misa.csv')
# ts.plot(dateStampDf, plot.type = "single", list(xlab="date", ylab=NULL), lty=c(1,2,1), col=c("blue", "red", "black"))
