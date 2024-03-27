#### PROJECT ANALYSIS 

#LOAD PACKAGES
require(tseries)
require(TSA)
require(forecast)
library(tibble)
library(dplyr)

### LOAD DATA AND ATTACH
a = Under_five_Mortality_Rates_2022_new[209,]
b = as.data.frame(a)
calli = t(b)
view(calli)
TRIM= calli[-c(1:3),]
view(TRIM)
Trim1 = data.frame(TRIM)
colnames(Trim1)= c("RATES")
Mortality_rate_in_Ghana = rownames_to_column(Trim1, var = "DATES")
Mortality_rate_in_Ghana = Mortality_rate_in_Ghana%>%
  mutate(RATES = as.numeric(RATES))
attach(Mortality_rate_in_Ghana)
RATES

### CONVERT TO TIME SERIES AND PLOT 
Ratets=ts(RATES,start = 1950.5,frequency = 1)
autoplot(Ratets,ylab="Rates",xlab="Year",main="RATES IN GHANA",col="red")

## DIVIDING THE DATA INTO TRAINDATA AND TESTDATA
traindata=window(Ratets,start=1950,end=2011,frequency=1)
testdata = window(Ratets, start = 2012, end= 2021, frequency= 1)
traindata
testdata

### PLOT TRAINDATA
autoplot(traindata,ylab="Rates",xlab="Year",main="RATES IN GHANA",col="red")

#### TEST FOR STATIONARITY ON TRAINDATA
adf.test(traindata)
pp.test(traindata)
kpss.test(traindata)

###FIRST DIFFERENCE OF TRAINDATA AND PLOT 
traind = diff(traindata)
autoplot(traind, ylab = "RATES",xlab = "YEAR",main = "U5MR IN GHANA", col= "blue")

#### TEST FOR STATIONARITY AFTER FIRST DIFFERENCE 
kpss.test(traind)
adf.test(traind)
pp.test(traind)


#### ACF AND PACF PLOTS
Acf(traind)
Pacf(traind)

### SECOND DIFFERENCING AND PLOTS
Traindd = diff(traind)
autoplot(Traindd, xlab = "YEAR", ylab = "RATES", main = " SECOND DIFFERENCED PLOT", col= "violet")

### TEST FOR STATIONARITY AFTER SECOND DIFFERENCE
adf.test(Traindd)
PP.test(Traindd)
kpss.test(Traindd)

#### COMPETING MODELS FOR THE FIRST DIFFERENCE
S=auto.arima(traindata,d=1,ic="aic",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)
S=auto.arima(traindata,d=1,ic="aicc",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)
S=auto.arima(traindata,d=1,ic="bic",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)

#### COMPETING MODELS FOR SECOND DIFFERENCE 
S=auto.arima(traindata,d=2,ic="aic",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)
S=auto.arima(traindata,d=2,ic="aicc",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)
S=auto.arima(traindata,d=2,ic="bic",max.p=10,max.q=10,max.order=5,nmodels = 5,trace=TRUE,stepwise = FALSE,approximation = FALSE,start.p = 5,start.q = 5,stationary = FALSE)

#### SELECTING BEST MODEL FOR THE FIRST DIFFERENCE
Model=auto.arima(traindata,stepwise = FALSE,approximation=FALSE)
Model

####SELECING BEST MODEL FOR THE SECOND DIFFERENCE
Model1 = auto.arima(traindata, d = 2, stepwise = FALSE, approximation = FALSE)
Model1

####FORECASTS 
F1= forecast(Model,h=10)
F1

F2=forecast(Model1,h=10)
F2
### TEST FOR ASSUMPTIONS
checkresiduals(Model)
checkresiduals(Model1)
shapiro.test(residuals(Model))
shapiro.test(residuals(Model1))

####CHECK ACCURACY
accuracy(F1,testdata)
accuracy(F2,testdata)

