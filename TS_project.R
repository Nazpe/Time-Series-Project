##############        Séries Temporais            #######################
##############           2021 /22                   #####################
##############  Grupo : Ana Rita Cheganças Nº106433 #####################
##############          Nuno Pedrosa Nº             #####################

# alguns sites interessantes...
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# https://www.geeksforgeeks.org/time-series-analysis-in-r/

# no dia antes de entregar fazer download do csv e atualizar que aquilo Ã© atualizado regularmente  

###############     LIBRARY      #########################
library(car)
library(tseries)


###############   LOADING AND TRANSFORMING THE DATA   #########################

#Path Rita
Data <- read.csv(file = 'C:/Users/Rita/Desktop/Mestrado em Ciência de Dados - UA/1º Ano/2º Semestre/Séries Temporais/Trabalho Grupo/Time-Series-Project/brent_oil.csv')
#Path Nuno
#Data <- read.csv(file = 'C:/Users/nunop/Desktop/C. Dados/Semestre 2/ST/Trabalho/git/Time-Series-Project/brent_oil.csv')
Data

# number of rows
nrow(Data)

# check first and last values
head(Data,1)
tail(Data,1)

With_date = subset(Data, select = -c(Open,High,Low,Volume,Currency) )
With_date

#plot(With_date, ylab = "Date", xlab = "Close")

#Final data
rownames(Data) <- Data[,1]
Data = subset(Data, select = -c(Date,Open,High,Low,Volume,Currency) )
Data

###############   EXAMINING THE DATA   #########################

#Descriptive statistics
summary(Data[[1]])
#Standard deviation and variance
sd(Data[[1]])
var(Data[[1]])

#QQPlot
#A analise do QQplot permite aferir a normalidade das variáveis a nível de representação gráfica
#permite comparar a distribuição dos nossos dados com uma distribuição normal

qqnorm(Data[[1]], main="Normal QQPlot for Oil Prices by Brent Barrel")
qqline(Data[[1]], col = "steelblue", lwd = 3, lty = 2)

#Histogram
hist(Data[[1]], main="Histogram for Oil Prices by Brent Barrel",
     xlab = "Oil Prices", col="grey",
     xlim=c(0,160)) 


###############   WHITE NOISE VS RANDOM WALK   #########################

#Plot the data 
plot.ts(Data, main="Oil Prices by Brent Barrel", ylab = "Oil Prices", xlab = "Date")
#?plot.ts




###############     STATIONARITY     #########################
adf.test(Data[[1]])




###############     SEASONALITY     #########################
#Model Additive
ts_Data_Add = ts(Data, frequency = 4)
DataComposeAdd <- decompose(ts_Data_Add, "additive")
DataComposeAdd

plot(as.ts(DataComposeAdd$seasonal))
plot(as.ts(DataComposeAdd$trend))
plot(as.ts(DataComposeAdd$random))
plot(DataComposeAdd)


#Model Multiplicative
ts_Data_Multi = ts(Data, frequency = 12)
DataComposeMulti <- decompose(ts_Data_Multi, "multiplicative")
DataComposeMulti

plot(as.ts(DataComposeMulti$seasonal))
plot(as.ts(DataComposeMulti$trend))
plot(as.ts(DataComposeMulti$random))
plot(DataComposeMulti)




###############   ACF AND PACF   #########################

acf(Data, main="ACF")
acf(Data, main="ACF with 1500 lags", lag=1500)

pacf(Data, main="PACF")
pacf(Data, main="PACF with 50 lags", lag=50)




###############   MODEL AR/MA/ARMA/ARIMA/SARIMA   ######################





###############      FORECASTING      #########################





