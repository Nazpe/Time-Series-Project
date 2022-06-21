#.......

# no dia antes de entregar fazer download do csv e atualizar que aquilo é atualizado regularmente  
Data <- read.csv(file = 'C:/Users/nunop/Desktop/C. Dados/Semestre 2/ST/Trabalho/git/Time-Series-Project/brent_oil.csv')
Data

# number of rows
nrow(Data)

# check first and last values
head(Data,1)
tail(Data,1)

With_date = subset(Data, select = -c(Open,High,Low,Volume,Currency) )
With_date

plot(With_date, ylab = "Date", xlab = "Close")

rownames(Data) <- Data[,1]
Data = subset(Data, select = -c(Date,Open,High,Low,Volume,Currency) )
Data


plot.ts(Data, main="Oil Prices by Brent Barrel")
?plot.ts

acf(Data)
acf(Data, lag=5768)
pacf(Data)
pacf(Data, lag=5768)

# alguns sites interessantes...
# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# https://www.geeksforgeeks.org/time-series-analysis-in-r/

