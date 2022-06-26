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
library(fpp2)
#library(sarima)



###############   LOADING AND TRANSFORMING THE DATA   #########################

#Path Rita
Data <- read.csv(file = 'C:/Users/Rita/Desktop/Mestrado em Ci�ncia de Dados - UA/1� Ano/2� Semestre/S�ries Temporais/Trabalho Grupo/Time-Series-Project/gold.csv')
#Path Nuno
#Data <- read.csv(file = 'C:/Users/nunop/Desktop/C. Dados/Semestre 2/ST/Trabalho/git/GIT/Time-Series-Project/gold.csv')
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
#A analise do QQplot permite aferir a normalidade das vari?veis a n?vel de representa??o gr?fica
#permite comparar a distribui??o dos nossos dados com uma distribui??o normal

qqnorm(DataList, main="Normal QQPlot for Gold Prices in USD")
qqline(DataList, col = "steelblue", lwd = 3, lty = 2)

# Não funciona não sei porque
#Histogram
hist(DataList, main="Histogram for Gold Prices in USD",
     xlab = "Gold Prices", col="grey",
     xlim=c(0,2500)) 


###NOTA: P-VALUE<5% - REJEITA-SE H0 - EXISTE ESTACIONARIEDADE - IND?CIOS DE WHITE NOISE
###      P-VALUE>5% - N?O REJEITA H0 - N?O EXISTE ESTACIONARIEDADE - IND?CIOS DE RANDOM WALK

###############   DATA EXPLORATION   #########################

mts <- ts(DataList, start = decimal_date(ymd("2000-01-04")),
          frequency = 255.333)

#Plot the data 
plot.ts(mts, main="Gold Prices in USD", ylab = "Gold Prices", xlab = "Date")
#?plot.ts

# First difference of the data
# Other way of considering the stabilization in mean: get the series with one difference
# Tirou a trend, ou seja deve ser em AR 1
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

# De agora a diante vamos apenas considerar o log Data e log e first difference Data, melhorou muito

###############     STATIONARITY     #########################
# site https://www.analyticsvidhya.com/blog/2021/06/statistical-tests-to-check-stationarity-in-time-series-part-1/

# So in summary, the ADF test has an alternate hypothesis of linear or difference stationary, while the KPSS test identifies trend-stationarity in a series.

?adf.test
adf.test(mtsl)
# Os dados log não são estacionários
# The p-value is obtained is greater than significance level of 0.05 
# Clearly, there is no reason to reject the null hypothesis. So, the time series is in fact non-stationary.

adf.test(mtslf)
# Os dados com log e primeiras diferenças são estacionários
# The p-value is obtained is smaller than significance level of 0.05 
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.


?kpss.test
kpss.test(mtsl, null = c("Level", "Trend"), lshort = TRUE)
# Os dados log são estacionários
# The p-value is obtained is smaller than significance level of 0.05 
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.

kpss.test(mtslf, null = c("Level", "Trend"), lshort = TRUE)
# Os dados com log e primeiras diferenças  são estacionários
# The p-value is obtained is smaller than significance level of 0.05 
# Clearly, we can reject the null hypothesis. So, the time series is in fact stationary.

# ADF test indicates your series does not have a unit root. This is not the same as stationarity. E.g. your series may have variance that is growing with time, a sinusoidal time trend or yet something else making it nonstationary. Something like that might be what the KPSS test is picking up.


###############     SEASONALITY     #########################

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

#### First differences

#Model Additive
#tsf_Data_Add = ts(fData, frequency = 258)
DataComposeAddf <- decompose(mtslf, "additive")
DataComposeAddf

plot(as.ts(DataComposeAddf$seasonal))
plot(as.ts(DataComposeAddf$trend))
plot(as.ts(DataComposeAddf$random))
plot(DataComposeAddf)


#Model Multiplicative
#tsf_Data_Multi = ts(fData, frequency = 258)
DataComposeMultif <- decompose(mtslf, "multiplicative")
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
#------------------#

par(mfrow=c(2,2))

acf(lData, 100, main="Log Data")
acf(lfData, 100, main="Log and First difference")

pacf(lData, 100, main="Log Data")
pacf(lfData, 100, main="Log and First difference")

# isto pode ser modelos arima, nesse caso não se pode concluir, mas sugere-se que:

# Para Data log, APENAS O PRIMEIRO VALOR (LAG=1) NO PACF ? SIGNIFICATIVO, LOGO, TEMOS O MODELO AR(1)
#APENAS O PRIMEIRO VALOR (LAG=1) NO PACF ? SIGNIFICATIVO, LOGO, TEMOS O MODELO AR(1)

# Para Log e Primeiras diferenças indica MA(1)

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


###############   MODEL AR/MA/ARMA/ARIMA/SARIMA   ######################


#AICc should especially be used when the ratio of your data points (n) : # of parameters (k) is < 40

??auto.arima
lDatafit_auto <- auto.arima(train_mtsl) # lData gives the same results ( if with split)
lDatafit_auto

#ARIMA(2,1,2) with drift 

#Coefficients:
#  ar1      ar2     ma1     ma2  drift
#-1.3697  -0.6872  1.3173  0.6160  3e-04
#s.e.   0.1465   0.1585  0.1584  0.1726  2e-04

#sigma^2 = 0.0001459:  log likelihood = 16644.45
#AIC=-33276.89   AICc=-33276.88   BIC=-33237.16

lfDatafit_auto <- auto.arima(train_mtslf) # lfData gives the same results ( if with split)
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

# experimentei um bocado e este parece ser mesmo o melhor
# equilibrio entre ser melhor e simples

#modelo com menor AIC o melhor com p value menor que 0.05

################## residual analysis ######################
# The “residuals” in a time series model are what is left over after fitting a model.
# Residuals are useful in checking whether a model has adequately captured the information in the data
# site https://otexts.com/fpp2/residuals.html

lres=residuals(lDatafit$fit)

lres
mean(lres)
var(lres)

lfres=residuals(lfDatafit$fit)

lfres
mean(lfres)
var(lfres)

??Box.Test

Box.test(lres, lag=10, type = "Ljung-Box")
#  based on Ljung-Box test, we don't accepted the null hypothesis  that the residuals are white noise. (no correlation)
Box.test(lfres, lag=10, type = "Ljung-Box")
#  based on Ljung-Box test, we accept the null hypothesis  that the residuals are white noise. (no correlation)

Box.test(lres, lag=10, type='Box-Pierce') #global test  for residuals noncorrelation
#p-value = 0.04201 reject no correlation 
Box.test(lfres, lag=10, type='Box-Pierce')
#p-value = 0.5185 don't reject no correlation 


?shapiro.test
# ve normalidade dos dados

shapiro.test(lres[0:5000]) 
# Log data não é normal p value < 2.2e-16
shapiro.test(lfres[0:5000]) 
# Log first difference data não é normal p value < 2.2e-16

?ks.test

ks.test(lres, pnorm,mean(lres),sqrt(var(lres)))
# Log data não é normal p value < 2.2e-16

ks.test(lfres, pnorm,mean(lres),sqrt(var(lres)))
# Log data não é normal p value < 2.2e-16


###############      FORECASTING      #########################

par(mfrow=c(1,1))

# Next 200 forecasted values
forecast(lDatafit_s, 200)

accuracy(forecast(lDatafit_s, 200), test_mtsl)

# plotting the graph with next
# 200 weekly forecasted values
plot(forecast(lDatafit_s, 200), main="Gold Prices in USD", ylab = "Prices", xlab = "Date")

# Next 200 forecasted values
forecast(lfDatafit_s, 200)

accuracy(forecast(lfDatafit_s, 200), test_mtslf)

# plotting the graph with next
# 200 forecasted values
plot(forecast(lfDatafit_s, 200), main="Gold Prices in USD", ylab = "Prices", xlab = "Date")

#  it is very useful to transform a variable and hence to obtain a new variable that follows a normal distribution.
BoxCox.lambda(Data[[1]])
# Não percebi bem mas pronto xD

lprev<-sarima.for(train_mtsl,10,1,1,2)

lprev


lfprev<-sarima.for(train_mtslf,10,3,0,3)

lfprev


##################### Exponential Smothing methods for forecast ###############################

#### Esta parte tá só um monte de copiar colar randoms talvez se ignore ####

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
aus1 <- hw(ts(Data, frequency = 12)) #additive seas
aus2 <- hw(ts(Data, frequency = 12), seasonal="mult")
aus3 <- hw(ts(Data, frequency = 12), seasonal="mult",damped=TRUE)
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


