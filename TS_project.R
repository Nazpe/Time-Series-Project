##############        Time Series            #######################
##############           2021 /22                   #####################
##############  Grupo : Ana Rita Chegan?as N?106433 #####################
##############          Nuno Pedrosa N?94471             #####################

# alguns sites interessantes...
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# https://www.geeksforgeeks.org/time-series-analysis-in-r/

# no dia antes de entregar fazer download do csv e atualizar que aquilo é atualizado regularmente  
# dataset: https://www.kaggle.com/datasets/psycon/historical-brent-oil-price-from-2000-to-202204?fbclid=IwAR3ivHyObwmquClyDICxJVM9UoehqR0dVTlWXDJLOVe_K2MmDpFnrUG00Jo

###############     LIBRARY      #########################

library(car)
library(tseries)
library(astsa)
library(forecast)
library(lubridate)


###############   LOADING AND TRANSFORMING THE DATA   #########################

#Path Rita
Data <- read.csv(file = 'C:/Users/Rita/Desktop/Mestrado em Ci?ncia de Dados - UA/1? Ano/2? Semestre/S?ries Temporais/Trabalho Grupo/Time-Series-Project/brent_oil.csv')
#Path Nuno
Data <- read.csv(file = 'C:/Users/nunop/Desktop/C. Dados/Semestre 2/ST/Trabalho/git/GIT/Time-Series-Project/brent_oil.csv')
Data

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
# let's continue with 258 values per year, because is the most usual vallue

#plot(With_date, ylab = "Date", xlab = "Close")

#Final data
rownames(Data) <- Data[,1]
Data = subset(Data, select = -c(Date,Open,High,Low,Volume,Currency) )
Data

# Data in a List
DataList = Data$Close
DataList

###############   EXAMINING THE DATA   #########################

#Descriptive statistics
summary(Data[[1]])
#Standard deviation and variance
sd(Data[[1]])
var(Data[[1]])

#QQPlot
#A analise do QQplot permite aferir a normalidade das vari?veis a n?vel de representa??o gr?fica
#permite comparar a distribui??o dos nossos dados com uma distribui??o normal


qqnorm(Data[[1]], main="Normal QQPlot for Oil Prices by Brent Barrel")
qqline(Data[[1]], col = "steelblue", lwd = 3, lty = 2)

#Histogram
hist(Data[[1]], main="Histogram for Oil Prices by Brent Barrel",
     xlab = "Oil Prices", col="grey",
     xlim=c(0,160)) 


###NOTA: P-VALUE<5% - REJEITA-SE H0 - EXISTE ESTACIONARIEDADE - IND?CIOS DE WHITE NOISE
###      P-VALUE>5% - N?O REJEITA H0 - N?O EXISTE ESTACIONARIEDADE - IND?CIOS DE RANDOM WALK

###############   DATA EXPLORATION   #########################

mts <- ts(DataList, start = decimal_date(ymd("2000-01-04")),
          frequency = 256.7273)

#Plot the data 
plot.ts(mts, main="Oil Prices by Brent Barrel", ylab = "Oil Prices", xlab = "Date")
#?plot.ts

# será que deviamos tirar a trend aos dados? Isto tira?
# First difference of the data
# Other way of considering the stabilization in mean: get the series with one difference
# Tirou a trend, ou seja deve ser em AR 1
fData=diff(DataList)

par(mfrow=c(1,1))

mtsf <- ts(fData, start = decimal_date(ymd("2000-01-04")),
          frequency = 256.7273)

plot.ts(mtsf, main="Oil Prices by Brent Barrel", ylab = "Oil Prices", xlab = "Date")

mean(fData)
var(fData)
plot.ts(fData)

# O Log não faz sentido, não é exponencial

###############     STATIONARITY     #########################
# site https://www.analyticsvidhya.com/blog/2021/06/statistical-tests-to-check-stationarity-in-time-series-part-1/

# So in summary, the ADF test has an alternate hypothesis of linear or difference stationary, while the KPSS test identifies trend-stationarity in a series.

?adf.test
adf.test(mts)
# Os dados originais não são estacionários
# The p-value is obtained is greater than significance level of 0.05 
# Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

adf.test(mtsf)
# Os dados com primeiras diferenças são estacionários
# The p-value is obtained is smaller than significance level of 0.05 
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.


?kpss.test
kpss.test(mts, null = c("Level", "Trend"), lshort = TRUE)
# Os dados normais são estacionários
# The p-value is obtained is smaller than significance level of 0.05 
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.

kpss.test(mtsf, null = c("Level", "Trend"), lshort = TRUE)
# Os dados com primeiras diferenças não são estacionários
# The p-value is obtained is greater than significance level of 0.05 
# Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

# ADF test indicates your series does not have a unit root. This is not the same as stationarity. E.g. your series may have variance that is growing with time, a sinusoidal time trend or yet something else making it nonstationary. Something like that might be what the KPSS test is picking up.


###############     SEASONALITY     #########################

#### Original Data

#Model Additive
#ts_Data_Add = ts(Data, frequency = 258)
DataComposeAdd <- decompose(mts, "additive")
DataComposeAdd

plot(as.ts(DataComposeAdd$seasonal))
plot(as.ts(DataComposeAdd$trend))
plot(as.ts(DataComposeAdd$random))
plot(DataComposeAdd)


#Model Multiplicative
#ts_Data_Multi = ts(Data, frequency = 258)
DataComposeMulti <- decompose(mts, "multiplicative")
DataComposeMulti

plot(as.ts(DataComposeMulti$seasonal))
plot(as.ts(DataComposeMulti$trend))
plot(as.ts(DataComposeMulti$random))
plot(DataComposeMulti)

#### First differences

#Model Additive
#tsf_Data_Add = ts(fData, frequency = 258)
DataComposeAddf <- decompose(mtsf, "additive")
DataComposeAddf

plot(as.ts(DataComposeAddf$seasonal))
plot(as.ts(DataComposeAddf$trend))
plot(as.ts(DataComposeAddf$random))
plot(DataComposeAddf)


#Model Multiplicative
#tsf_Data_Multi = ts(fData, frequency = 258)
DataComposeMultif <- decompose(mtsf, "multiplicative")
DataComposeMultif

plot(as.ts(DataComposeMultif$seasonal))
plot(as.ts(DataComposeMultif$trend))
plot(as.ts(DataComposeMultif$random))
plot(DataComposeMultif)

## NOTA: ACF C/ N? LAGS MENOR AT? VALOR DE 0 - MODELO MA
##       PACF C/ N? LAGS MENOR AT? VALOR DE 0 - MODELO AR

###############   ACF AND PACF   #########################

#------------------#
acf2(DataList)
acf2(fData)
# perguntar stora
#------------------#

par(mfrow=c(2,2))

acf(DataList, 50, main="Global Data")
acf(diff(DataList), 50, main="First difference")

pacf(DataList, 50, main="Global Data")
pacf(diff(DataList), 50, main="First difference")

# Para Data normal, APENAS O PRIMEIRO VALOR (LAG=1) NO PACF ? SIGNIFICATIVO, LOGO, TEMOS O MODELO AR(1)
#APENAS O PRIMEIRO VALOR (LAG=1) NO PACF ? SIGNIFICATIVO, LOGO, TEMOS O MODELO AR(1)

# Para Primeiras diferenças indica MA(1)

###############   MODEL AR/MA/ARMA/ARIMA/SARIMA   ######################
DataList
mts


??auto.arima
Datafit_auto <- auto.arima(ts_Data_Add)
Datafit_auto

# Next 5 forecasted values
forecast(Datafit_auto, 5)

# plotting the graph with next
# 5 weekly forecasted values
plot(forecast(Datafit_auto, 5), main="Oil Prices by Brent Barrel", ylab = "Oil Prices", xlab = "Date")




fit=sarima(Data[[1]],1,1,0,0,0,0,0) 
fit# look at the significance of estimates 
#constant is not significant since p.value=0.3605
#AIC = 3.53656

fit=sarima(Data[[1]],1,1,0,0,0,0,0, no.constant=TRUE)
fit
#AIC = 3.536359

z=c(1,-0.24,-0.19) #aqui as raizes tem de ser as da parte AR do modelo (tipo a parte pratica que fizemos na aula)
raizes=polyroot(z)
raizes

#modelo com menor AIC ? o melhor

#residual analysis
#look at the plots
summary(sarima(Data[[1]],1,1,0,0,0,0,0, no.constant=TRUE)$fit)

res=residuals(sarima(Data[[1]],1,1,0,0,0,0,0, no.constant=TRUE)$fit)

res
mean(res)
var(res)

Box.test(res,lag=10, type='Box-Pierce') #global test until lag=10,  for residuals noncorrelation
#p-value = 0.002972 reject no correlation (till lag 10)

shapiro.test(res[0:5000]) # normality test p-value =0.4494>0.05

# este dá um erro com os nossos dados não sei porque???
ks.test(res, pnorm,mean(res),sqrt(var(res)))
ks.test(res[0:5000], pnorm)


###############      FORECASTING      #########################

BoxCox.lambda(Data[[1]])

require("astsa")

prev<-sarima.for(Data,12,0,1,1,0,1,1,12) # tem de se mudar os valores
?sarima.for

prev

exp(prev$pred)

#fcast<-forecast.arima(smodel, lambda=0)

library(forecast)
library(fpp2)
library(fpp)

fit <- auto.arima(Data, max.P=5,max.Q=3,D=1)
fit

ndiff() #number of differences that we shoud use
nsdiff() #number of differences in seasonal component that we shoud use

auto.arima(Data, , lambda=0)
fit<-auto.arima(Data, lambda=0)
fcast<-forecast.arima(fit, lambda=0)

##################### Exponential Smothing methods for forecast ###############################

# Esta parte tá só um monte de copiar colar randoms

install.packages("fpp", dependencies=TRUE)
library(fpp)

# se tivesse no trend
fit <- ses(DataList, h=20)#forecasts 5 obs, using simple exponential smoothing
plot(fit)
summary(fit)#show the model and forecasts

# linear trend
par(mfrow=c(3,1))
plot(DataList)
fit2 <- holt(DataList, h=20)  
plot(fit2)
summary(fit2)

#or alternatively
fit_ets2=ets(DataList, model="AAN")
#Smoothing parameters: alpha = 0.9999 ,beta  = 1e-04,very small
#so, would it be better not to consider a trend  


#What is better?compare AIC and RMSE 

#plot(stl(fit_ets2$model, s.window = "per"))#gives the decomposition
plot(fit_ets2$model)

par(mfrow=c(1,1))

# exponential smoothing with/and without linear trend

fit3 <- holt(DataList)

plot(fit3$model)#decomposition plot

fit3$model #fit
summary(fit3) #fit with forecasts

par(mfrow=c(2,1))

plot(fit3)
#or
plot(fit3)#with forecasts
lines(fitted(fit3), col="red")

fit31 <- ses(DataList)
summary(fit31)

plot(fit31$model) #decomposition 


plot(fit31)
lines(fitted(fit31), col="red")


accuracy(fit31)
accuracy(fit3)

# Holt_winters (trend+seasonal)

par(mfrow=c(4,1))
plot(DataList)

# esta frequancia não dá não sei porque
aus1 <- hw(ts(Data, frequency = 258)) #additive seas
aus2 <- hw(ts(Data, frequency = 258), seasonal="mult")
aus3 <- hw(ts(Data, frequency = 258), seasonal="mult",damped=TRUE)
summary(aus1)
summary(aus2)
summary(aus3);

plot(aus1)
plot(aus2)
plot(aus3)

plot(aus2$model)

##2a
fit1_ausbeer <- ets(ts(Data, frequency = 12),model="AAA",damped=FALSE)##Holt-Winters addtive errors, trend and seas. 
fit1_ausbeer
#    AIC     AICc      BIC 
#2315.264 2316.159 2345.430


#OBS when it is used ets function, plot(model) does the decomposition
plot(fit1_ausbeer)

fcast1 <- forecast(fit1_ausbeer, h=20)
fcast1

plot(fcast1)
lines(fitted(fcast1), col="red")

accuracy(fcast1)#RMSE  15.92394 

ou
plot(fit1_ausbeer)

##2b
fit2_ausbeer <- ets(ts(Data, frequency = 12),model="MAM",damped=FALSE)##Holt-Winters mult: errors and seas; addit trend
#     AIC     AICc      BIC 
#2273.619 2274.515 2303.786  #best fit

fcast2 <- forecast(fit2_ausbeer, h=20)
plot(fcast2)
lines(fitted(fcast1), col="red")
accuracy(fcast2)# RMSE 15.65815  , better accuracy

##2c
fit_aut<- ets(ausbeer)#automatic fit
summary(fit)


plot(fit_aut)#does the decomposition


fcast_aut<- forecast(fit_aut, h=20)

plot(fcast_aut)
lines(fitted(fcast_aut), col="red")

#ETS(M,A,M) 

res_ets_aut <- residuals(fit_aut)
plot(res_ets_aut)


