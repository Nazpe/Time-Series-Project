##############        Time Series            #######################
##############           2021 /22                   #####################
##############  Grupo : Ana Rita Chegan?as N?106433 #####################
##############          Nuno Pedrosa N?94471             #####################


###############     LIBRARY      #########################

library(car)
library(tseries)
library(astsa)
library(forecast)
library(lubridate)
library(ffp)
library(fpp2)
library(sarima)



###############   LOADING AND TRANSFORMING THE DATA   #########################

# These paths are the authors paths, please add your corresponding path to the data.
#Path Rita
#Data <- read.csv(file = 'C:/Users/Rita/Desktop/Mestrado em Ci?ncia de Dados - UA/1? Ano/2? Semestre/S?ries Temporais/Trabalho Grupo/Time-Series-Project/gold.csv')
#Path Nuno
#Data <- read.csv(file = 'C:/Users/nunop/Desktop/C. Dados/Semestre 2/ST/Trabalho/git/GIT/Time-Series-Project/gold.csv')
View(Data)

# number of rows
nrow(Data)

# check first and last values
head(Data,1)
tail(Data,1)

# check days by year ( Because the stock market isn't open everyday we don't have 365 values per year)
With_date = subset(Data, select = -c(Open,High,Low,Volume,Currency) )
With_date
Dates = With_date$Date
Dates
tail(Dates,200)

years_list <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
years_list
length(years_list)
years_count <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
years_count
years_list

for(i in Dates) {
  year = strsplit(i, split = "-")[[1]][1]
  id = which(year == years_list)
  years_count[id] = years_count[id] + 1
  
}

years_count
years_count = head(years_count, -1)
years_count
mean(years_count)
# let's continue with 255 values per year, is the mean of values by year


#Final data
Data
rownames(Data) <- Data[,1]
Data = subset(Data, select = -c(Date,Open,High,Low,Volume,Currency) )
Data

# Data in a List
DataList = Data$Close
DataList

###############   EXAMINING THE DATA   #########################

#Descriptive statistics
summary(DataList)
#Standard deviation and variance
sd(DataList)
var(DataList)

#QQPlot

qqnorm(DataList, main="Normal QQPlot for Gold Prices in USD")
qqline(DataList, col = "steelblue", lwd = 3, lty = 2)

#Histogram
hist(DataList, main="Histogram for Gold Prices in USD",
     xlab = "Gold Prices", col="grey",
     xlim=c(0,2500)) 

###############   DATA EXPLORATION   #########################

mts <- ts(DataList, start = decimal_date(ymd("2000-01-04")),
          frequency = 255.333)

#Plot the data 
plot.ts(mts, main="Gold Prices in USD", ylab = "Gold Prices", xlab = "Date")
#?plot.ts

# First difference of the data and log
fData=diff(DataList)
lData=log(DataList)
lfData=diff(log(DataList))

mtsf <- ts(fData, start = decimal_date(ymd("2000-01-04")),
          frequency = 255.333)

mtsl <- ts(lData, start = decimal_date(ymd("2000-01-04")),
           frequency = 255.333)

mtslf <- ts(lfData, start = decimal_date(ymd("2000-01-04")),
           frequency = 255.333)

mean(DataList)
var(DataList)

mean(fData)
var(fData)

mean(lData)
var(lData)

mean(lfData)
var(lfData)

par(mfrow=c(2,2))

plot.ts(mts, main="Normal", ylab = "Gold Prices", xlab = "Date")
plot.ts(mtsf, main="First Differences", ylab = "Gold Prices", xlab = "Date")
plot.ts(mtsl, main="Log", ylab = "Gold Prices", xlab = "Date")
plot.ts(mtslf, main="Log and First Differences", ylab = "Gold Prices", xlab = "Date")

# Now we will consider log Data and log and first difference Data

###############     STATIONARITY     #########################

adf.test(mtsl)
# Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

adf.test(mtslf)
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.


#?kpss.test
kpss.test(mtsl, null = c("Level", "Trend"), lshort = TRUE)
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.

kpss.test(mtslf, null = c("Level", "Trend"), lshort = TRUE)
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.

###############     DECOMPOSE    #########################

#### Log Data

#Model Additive
DataComposeAdd <- decompose(mtsl, "additive")
DataComposeAdd

plot(as.ts(DataComposeAdd$seasonal))
plot(as.ts(DataComposeAdd$trend))
plot(as.ts(DataComposeAdd$random))
plot(DataComposeAdd)


#Model Multiplicative
DataComposeMulti <- decompose(mtsl, "multiplicative")
DataComposeMulti

plot(as.ts(DataComposeMulti$seasonal))
plot(as.ts(DataComposeMulti$trend))
plot(as.ts(DataComposeMulti$random))
plot(DataComposeMulti)

#### Log and First differences

#Model Additive
DataComposeAddf <- decompose(mtslf, "additive")
DataComposeAddf

plot(as.ts(DataComposeAddf$seasonal))
plot(as.ts(DataComposeAddf$trend))
plot(as.ts(DataComposeAddf$random))
plot(DataComposeAddf)


#Model Multiplicative
DataComposeMultif <- decompose(mtslf, "multiplicative")
DataComposeMultif

plot(as.ts(DataComposeMultif$seasonal))
plot(as.ts(DataComposeMultif$trend))
plot(as.ts(DataComposeMultif$random))
plot(DataComposeMultif)


############### Train Test Split #################

# As our data is very irregular, we can't have a big test data (values very far way from the last know value will have a higher chance of being wrong)
# We will use the last 200 entrys for test set (about a year)

test_mts = tail(mts,200)
test_mts
train_mts = head(mts, length(mts) - 200)
train_mts

test_mtsf = tail(mtsf,200)
test_mtsf
train_mtsf = head(mtsf, length(mts) - 200)
train_mtsf

test_mtsl = tail(mtsl,200)
test_mtsl
train_mtsl = head(mtsl, length(mts) - 200)
train_mtsl

test_mtslf = tail(mtslf,200)
test_mtslf
train_mtslf = head(mtslf, length(mts) - 200)
train_mtslf

par(mfrow=c(2,2))

plot.ts(train_mts, main="Normal", ylab = "Gold Prices", xlab = "Date")
plot.ts(train_mtsf, main="First Differences", ylab = "Gold Prices", xlab = "Date")
plot.ts(train_mtsl, main="Log", ylab = "Gold Prices", xlab = "Date")
plot.ts(train_mtslf, main="Log and First Differences", ylab = "Gold Prices", xlab = "Date")

###############   ACF AND PACF   #########################

par(mfrow=c(2,2))

acf(train_mtsl, 100, main="Log Data")
acf(train_mtslf, 100, main="Log and First difference")

pacf(train_mtsl, 100, main="Log Data")
pacf(train_mtslf, 100, main="Log and First difference")

###############   MODEL AR/MA/ARMA/ARIMA/SARIMA   ######################

#??auto.arima
lDatafit_auto <- auto.arima(train_mtsl) 
lDatafit_auto

#ARIMA(2,1,2) with drift 

#Coefficients:
#  ar1      ar2     ma1     ma2  drift
#-1.3697  -0.6872  1.3173  0.6160  3e-04
#s.e.   0.1465   0.1585  0.1584  0.1726  2e-04

#sigma^2 = 0.0001459:  log likelihood = 16644.45
#AIC=-33276.89   AICc=-33276.88   BIC=-33237.16

lfDatafit_auto <- auto.arima(train_mtslf) 
lfDatafit_auto

#ARIMA(3,0,4) with non-zero mean 

#Coefficients:
#  ar1     ar2     ar3     ma1      ma2      ma3     ma4   mean
#-0.6147  0.5880  0.8093  0.5472  -0.6499  -0.7668  0.0275  3e-04
#s.e.   0.0651  0.0571  0.0578  0.0664   0.0572   0.0593  0.0177  1e-04

#sigma^2 = 0.0001455:  log likelihood = 16655.72
#AIC=-33293.44   AICc=-33293.41   BIC=-33233.84

# Log Data
?sarima

lDatafit=sarima(train_mtsl,1,1,2,0,0,0,0) 
lDatafit


#Coefficients:
#  ar1     ma1      ma2  constant
#-0.7656  0.6979  -0.0901     3e-04
#s.e.   0.0800  0.0803   0.0133     1e-04

#sigma^2 estimated as 0.000146:  log likelihood = 16640.32,  aic = -33270.65

#$degrees_of_freedom
#[1] 5548

#$ttable
#Estimate     SE t.value p.value
#ar1       -0.7656 0.0800 -9.5702  0.0000
#ma1        0.6979 0.0803  8.6894  0.0000
#ma2       -0.0901 0.0133 -6.7890  0.0000
#constant   0.0003 0.0001  2.2470  0.0247

#$AIC
#[1] -5.992552

#$AICc
#[1] -5.992551

#$BIC
#[1] -5.986589


# experimentei um bocado e este parece ser mesmo o melhor

#Log and first differences

lfDatafit=sarima(train_mtslf,3,0,3,0,0,0,0) 
lfDatafit

#Coefficients:
#  ar1     ar2     ar3     ma1      ma2      ma3  xmean
#-0.6040  0.5219  0.7444  0.5476  -0.5743  -0.7172  3e-04
#s.e.   0.1188  0.1378  0.0867  0.1170   0.1214   0.0830  1e-04

#sigma^2 estimated as 0.0001454:  log likelihood = 16654.59,  aic = -33293.18

#$degrees_of_freedom
#[1] 5546

#$ttable
#Estimate     SE t.value p.value
#ar1    -0.6040 0.1188 -5.0847  0.0000
#ar2     0.5219 0.1378  3.7871  0.0002
#ar3     0.7444 0.0867  8.5831  0.0000
#ma1     0.5476 0.1170  4.6814  0.0000
#ma2    -0.5743 0.1214 -4.7297  0.0000
#ma3    -0.7172 0.0830 -8.6434  0.0000
#xmean   0.0003 0.0001  2.6783  0.0074

#$AIC
#[1] -5.99553

#$AICc
#[1] -5.995526

#$BIC
#[1] -5.98599

lDatafit_s=Arima(train_mtsl, order=c(1,1,2)) 
lDatafit_s
summary(lDatafit_s$fit)

lfDatafit_s=Arima(train_mtslf, order=c(3,0,3)) 
lfDatafit_s
summary(lfDatafit_s$fit)

############        RESIDUAL ANALYSIS FOR ARIMA MODEL          ######################
# The residuals in a time series model are what is left over after fitting a model.
# Residuals are useful in checking whether a model has adequately captured the information in the data

lres=residuals(lDatafit$fit)
lres
mean(lres)
var(lres)

lfres=residuals(lfDatafit$fit)
lfres
mean(lfres)
var(lfres)

Box.test(lres, lag=10, type = "Ljung-Box")
#  based on Ljung-Box test, we don't accepted the null hypothesis  that the residuals are white noise. 
Box.test(lfres, lag=10, type = "Ljung-Box")
#  based on Ljung-Box test, we accept the null hypothesis  that the residuals are white noise. 

Box.test(lres, lag=10, type='Box-Pierce') 
#p-value = 0.04201 reject no correlation 
Box.test(lfres, lag=10, type='Box-Pierce')
#p-value = 0.5185 don't reject no correlation 


shapiro.test(lres[0:5000]) 
# Log data isn't normal
shapiro.test(lfres[0:5000]) 
# Log first difference data ins't normal

#?ks.test

ks.test(lres, pnorm,mean(lres),sqrt(var(lres)))
# Log data isn't normal
ks.test(lfres, pnorm,mean(lfres),sqrt(var(lfres)))
# Log first difference data ins't normal

##############       EXPONENTIAL SMOTHING METHODS      ###############################

#dataset log
lETSDatafit_s = ets(train_mtsl)
print(summary(lETSDatafit_s))
checkresiduals(lETSDatafit_s)

#residual analysis - log
lETSres=residuals(lETSDatafit_s)
lETSres
mean(lETSres)
var(lETSres)
Box.test(lETSres, lag=10, type = "Ljung-Box")
Box.test(lETSres, lag=10, type='Box-Pierce')
shapiro.test(lETSres[0:5000]) 
ks.test(lETSres, pnorm,mean(lETSres),sqrt(var(lETSres)))

#dataset log and first differences
lfETSDatafit_s = ets(train_mtslf)
print(summary(lfETSDatafit_s))
checkresiduals(lfETSDatafit_s)

#residual analysis - log and first differences
lfETSres=residuals(lfETSDatafit_s)
lfETSres
mean(lfETSres)
var(lfETSres)
Box.test(lfETSres, lag=10, type = "Ljung-Box")
Box.test(lfETSres, lag=10, type='Box-Pierce')
shapiro.test(lfETSres[0:5000]) 
ks.test(lfETSres, pnorm,mean(lfETSres),sqrt(var(lfETSres)))

###############         FORECASTING            #########################

par(mfrow=c(1,1))

# Next 200 forecasted values
forecast(lDatafit_s, 200)
accuracy(forecast(lDatafit_s, 200), test_mtsl)
# plotting the graph with next
# 200 weekly forecasted values
plot(forecast(lDatafit_s, 200), main="Prediction Gold Prices for log dataset (ARIMA MODEL)", ylab = "Prices", xlab = "Date", xlim=c(2018, 2023))

# Next 200 forecasted values
forecast(lfDatafit_s, 200)
accuracy(forecast(lfDatafit_s, 200), test_mtslf)
# plotting the graph with next
# 200 forecasted values
plot(forecast(lfDatafit_s, 200), main="Prediction Gold Prices for log and first difference dataset (ARIMA MODEL)", ylab = "Prices", xlab = "Date", xlim=c(2018, 2023))

# Next 200 forecasted values
forecast(lETSDatafit_s, 200)
accuracy(forecast(lETSDatafit_s, 200), test_mtsl)
# plotting the graph with next
# 200 forecasted values
plot(forecast(lETSDatafit_s, 200), main="Prediction Gold Prices for log dataset (ETS MODEL)", ylab = "Prices", xlab = "Date", xlim=c(2018, 2023))

# Next 200 forecasted values
forecast(lfETSDatafit_s, 200)
accuracy(forecast(lfETSDatafit_s, 200), test_mtslf)
# plotting the graph with next
# 200 forecasted values
plot(forecast(lfETSDatafit_s, 200), main="Prediction Gold Prices for log and first difference dataset (ETS MODEL)", ylab = "Prices", xlab = "Date", xlim=c(2018, 2023))


