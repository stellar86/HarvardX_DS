################################################################################################################################
#Author: E Rossouw
#Date: 2020-01-02
#Description: Bitcoin Analysis and Forecasting
################################################################################################################################

################################################################################################################################
# INSTRUCTIONS - ENSURE THAT crypto-markets.csv ARE IN THE SAME DIRECTORY AS THE .R FILE AND .RMD FILE
# FILE CAN BE DOWNLOADED WITH A REGISTERED ACCOUNT FROM: https://www.kaggle.com/jessevent/all-crypto-currencies
# Ensure that you have internet access for installing libraries!
# Installing libraries can take 30 minutes
# Set working directory to local path on computer (line106 in .R and line140 in .RMD)
# Install Tinytex if required: install_tinytex() -- accept DLL error missing (Can be done on line 33)
################################################################################################################################

################################################################################################################################
# Install Required Libraries
################################################################################################################################
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) install.packages("plotly")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(Metrics)) install.packages("Metrics")
if(!require(lubridate)) install.packages("lubridate")
if(!require(caret)) install.packages("caret")
if(!require(forcats)) install.packages("forcats")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(formatR)) install.packages("formatR")
if(!require(forecast)) install.packages("forecast", dependencies = TRUE)
if(!require(tseries)) install.packages("tseries")
if(!require(scales)) install.packages("scales")
if(!require(anytime)) install.packages("anytime")
if(!require(bsts)) install.packages("bsts")
if(!require(car)) install.packages("car")
if(!require(keras)) install.packages("keras")
if(!require(MCMCpack)) install.packages("MCMCpack")
if(!require(smooth)) install.packages("smooth")
if(!require(tensorflow)) install.packages("tensorflow")
if(!require(tseries)) install.packages("tseries")
if(!require(TTR)) install.packages("TTR")
if(!require(fpp2)) install.packages("fpp2")
if(!require(AnalyzeTS)) install.packages("AnalyzeTS")
if(!require(arm)) install.packages("arm")
if(!require(corrplot)) install.packages("corrplot")
if(!require(kernlab)) install.packages("kernlab")
if(!require(gam)) install.packages("gam")
if(!require(randomForest)) install.packages("randomForest")

################################################################################################################################
# Load Required Libraries
################################################################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(Metrics)
library(lubridate)
library(forcats)
library(knitr)
library(kableExtra)
library(formatR)
library(forecast)
library(tseries)
library(scales)
library(grid)
library(anytime)
library(bsts)
library(car)
library(forecast)
library(keras)
library(MCMCpack)
library(smooth)
library(tseries)
library(TTR)
library(fpp2)
library(caret)
library(AnalyzeTS)
library(arm)
library(corrplot)
library(kernlab)
library(gam)
library(randomForest)

################################################################################################################################
# Create Crypto Data Sets
################################################################################################################################
#Crypto currency History Dataset can be downloaded fromhttps://www.kaggle.com/jessevent/all-crypto-currencies - crypto-markets.csv

#Create Datasets

# set the working directory to the main folder containing the project files
setwd( "D:/Google Drive/Personal/Edx/edx-dl-master/Downloaded/Capstone Project/Cryptos_Final/" )

#Ensure crypto-markets.csv is in the same directory as R file
data_all_markets <- read.csv('crypto-markets.csv')

#Create Bitcoin dataset for prediction
data_bitcoin <- data_all_markets %>% filter(symbol == 'BTC')

################################################################################################################################
# Global Functions
################################################################################################################################
#Styling for kable tables
kable_table <- function(data = NULL, titlex = NULL)
{
  #Update kable format to kable(data,format="latex") for RMarkdown
  kable(data) %>%
    kable_styling(bootstrap_options = c("striped", "hold_position"),
                  position = "center",
                  font_size = 10,
                  full_width = FALSE) %>%
    footnote(general = titlex,
             footnote_as_chunk = T, threeparttable=T)
}

#Convert difference between nth and (n-1)th element

convertPercent <- function(data){
  data= diff(data)/data[-NROW(data)] * 100
  return(data)
}

#RMSE Function for Analysis

RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

################################################################################################################################
# Section 2: Data Exploration
################################################################################################################################
#Check Dataset Composition
kable(head(data_all_markets,5)) %>%
  kable_styling(bootstrap_options = "striped", font_size = 8,full_width = F , position ="left",latex_options="scale_down") %>%
  footnote(general = "All Cryptos Columns Overview",
           footnote_as_chunk = T)

#Data Frame Structure Overview
glimpse(data_all_markets)

#Check if NA's exist in data sets
data_all_markets_na <- sapply(data_all_markets, function(x) sum(is.na(x)))
kable_table(data_all_markets_na,"Check NA's data_all_markets Dataset")

data_bitcoin_na <- sapply(data_bitcoin, function(x) sum(is.na(x)))
kable_table(data_bitcoin_na,"Check NA's data_bitcoin Dataset")

################################################################################################################################
# Section 3: Feature Engineering/Pre-Processing
################################################################################################################################
#Convert date from factor to character
data_all_markets$date <- ymd(as.character(data_all_markets$date))
data_bitcoin$date <- ymd(as.character(data_bitcoin$date))
glimpse(data_all_markets)
glimpse(data_bitcoin)

# Extract the year and month and day of crypto currency price
data_all_markets$priceYear <- format(data_all_markets$date,"%Y")
data_all_markets$priceMonth <- format(data_all_markets$date,"%m")
data_all_markets$priceDay <- format(data_all_markets$date,"%d")

data_bitcoin$priceYear <- format(data_bitcoin$date,"%Y")
data_bitcoin$priceMonth <- format(data_bitcoin$date,"%m")
data_bitcoin$priceDay<- format(data_bitcoin$date,"%d")

glimpse(data_all_markets)
glimpse(data_bitcoin)

#Convert slug,symbol and name to character fields
data_all_markets$slug <- as.character(data_all_markets$slug)
data_all_markets$symbol <- as.character(data_all_markets$symbol)
data_all_markets$name <- as.character(data_all_markets$name)

data_bitcoin$slug <- as.character(data_bitcoin$slug)
data_bitcoin$symbol <- as.character(data_bitcoin$symbol)
data_bitcoin$name <- as.character(data_bitcoin$name)

glimpse(data_all_markets)
glimpse(data_bitcoin)

################################################################################################################################
# Section 4: Data Analysis
################################################################################################################################
#Get Unique nr of crypto currencies in dataset
kable_table(length(unique(data_all_markets$symbol)),"Nr of Unique Crypto Currencies")

#Top 20 Crypto Currencies by Market Cap
data_all_markets %>% group_by(name) %>% summarise(market_cap = max(market)) %>% arrange(desc(market_cap)) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(name, -market_cap), y = market_cap)) + theme_economist_white() + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9)) +
  labs(title = "Market Capitalization of Top 20 Crypto's",
       x = "Crypto Currency",
       y = "Market Cap")

#Top 5 Crypto Currencies - Price Action over Time
data_all_markets %>% dplyr::select(close,name,date) %>% filter(name %in% c("Bitcoin","Ethereum","XRP","Bitcoin Cash","Cardano")) %>% 
  ggplot(aes(date,close,color = name)) + geom_line() + 
  theme(legend.position = "bottom",legend.text = element_text(size = 8, colour = "black")) +   
  labs(title = "Price Action of Top 5 Crypto's over Time",
       x = "Date",
       y = "Price USD")

#Top 5 Crypto Currencies - Market Capitalization over Time
data_all_markets %>% dplyr::select(market,name,date) %>% filter(name %in% c("Bitcoin","Ethereum","XRP","Bitcoin Cash","Cardano")) %>% 
  ggplot(aes(date,market,color = name)) + geom_line() + theme(legend.position = "bottom",legend.text = element_text(size = 8, colour = "black")) +
  labs(title = "Market Capitalization of Top 5 Crypto's over Time",
       x = "Date",
       y = "Market Capitalization")

#Price Action vs Market Capitalization for Bitcoin
data_all_markets %>% dplyr::select(market,close,name,date) %>% filter(name %in% c("Bitcoin")) %>% 
  ggplot(aes(market,close)) + geom_point() + geom_smooth(method = 'gam') + 
  theme_economist_white() +   
  labs(title = "Price Action vs Market Cap for Bitcoin",
       x = "Market Capitalization",
       y = "Bitcoin Price USD")

#Investigate Up and Down Months Bitcoin Price
data_bitcoin %>% group_by(priceYear, priceMonth) %>% summarise(open = first(open), close = last(close), spread = last(close) -  first(open)) %>% 
  ggplot(aes(priceMonth,spread, colour = spread > 0)) + geom_point() + facet_grid(priceYear ~ .) + theme(legend.position = "bottom",legend.text = element_text(size = 8, colour = "black")) +
  labs(title = "Up and Down Months",
       x = "Date",
       y = "Spread USD")

#Investigate Price Volatility in Bitcoin
data_all_markets %>% dplyr::select(market,spread,name,date,priceYear) %>% filter(name %in% c("Bitcoin") & priceYear == 2018 ) %>% 
  ggplot(aes(date,spread)) + geom_line() + 
  theme_economist_white() +   
  labs(title = "Price Volatility using Spread (2018)",
       x = "Date (2018)",
       y = "Bitcoin Spread USD")

#Determine trends and variability for time series data by decomposition of the time series data
#Create data set with the correct columns for the xts and ts functions
ts_data <- data_bitcoin %>% dplyr::select(date,open,high,low,close)
#Sort data by date
ts_data_sorted  <- xts(ts_data[, -1], order.by = (ts_data$date))
#Create Time-Series object since 2013
tsr_per <- ts(ts_data_sorted [,4], frequency = 365.25,start = c(2013,4,27))

#Determine Classical Seasonal Decomposition by Moving Averages
dec_tsr <- decompose(tsr_per) #Obtaining the trends and seasonality
plot(dec_tsr)

#Try to establish which are uptrend days
data_bitcoin %>% filter(priceYear >= 2018) %>% ggplot(aes(x=priceDay, y=spread, colour = spread > 500)) + facet_grid(priceMonth ~ .) +
  geom_point() + theme(legend.position = "bottom",legend.text = element_text(size = 8, colour = "black")) +
  labs(title = "Spread per day for 2018",
       x = "Day",
       y = "Spread USD")

#Determine $ increase in price based on market cap
set.seed(1, sample.kind = "Rounding")
doll_inc <-lm(close~market, data=data_bitcoin)
summary(doll_inc)

#Test Distribution's fit to normal distribution
qplot(data_bitcoin$volume, data_bitcoin$close) + ggtitle("Volume vs Closing Price") + theme_economist_white()

#Determine Correlation between price, volume and market cap,close_ratio and spread
cor_set <- cor(data_bitcoin[,9:13])
corrplot::corrplot(cor_set, method =  "color")

#Determine correlation between closing price and market cap
ACFdata <- data_bitcoin %>% dplyr::select(close,market)
btc_acf <- acf(ACFdata, na.action=na.pass, plot=FALSE)
glimpse(btc_acf)

plot(btc_acf)

################################################################################################################################
# Section 5: Create Training and Testing Data Partitions for Models
################################################################################################################################
set.seed(1, sample.kind="Rounding")

#Create smaller dataset for training and testing models
test_index<-sample(1:nrow(data_bitcoin),0.80*nrow(data_bitcoin), replace=FALSE)
train_set <- data_bitcoin[test_index,]
test_set <- data_bitcoin[-test_index,]

#Check if NA's exist in data sets
train_set_na <- sapply(train_set, function(x) sum(is.na(x)))
kable_table(train_set_na,"Check NA's train_set Dataset")

test_set_na <- sapply(test_set, function(x) sum(is.na(x)))
kable_table(test_set_na,"Check NA's test_set Dataset")

################################################################################################################################
# Section 5: Create Training and Testing Data Partitions for Models
################################################################################################################################
##################################Investigate Different of models##################################
set.seed(1, sample.kind="Rounding")
models <- c("glm", "svmLinear", "knn", "gamLoess","rf","bayesglm")

fits <- lapply(models, function(model){ 
  print(model)
  caret::train(close ~ market, method = model, data = train_set)
}) 

names(fits) <- models

#Predict Values on test_set
pred <- sapply(fits, function(object) 
  predict(object, newdata = test_set))

#Determine Accuracy
acc <- colMeans(pred == test_set$close)

#Print Accuracy
kable_table(acc,"Accuracy of various Models")

#Determine RMSE's
glm_rmse <-  RMSE(pred[,1],test_set$close)
svmLinear_rmse <-  RMSE(pred[,2],test_set$close)
knn_rmse <-  RMSE(pred[,3],test_set$close)
gamLoess_rmse <-  RMSE(pred[,4],test_set$close)
rf_rmse <-  RMSE(pred[,5],test_set$close)
bays_rmse <-  RMSE(pred[,6],test_set$close)

#Create Data Frame for RMSE's
rmse_results <- data.frame(Dataset = "Train/Test", Model="glm", RMSE=glm_rmse)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="svmLinear",RMSE = svmLinear_rmse)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="knn",RMSE = knn_rmse)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="gamLoess",RMSE = gamLoess_rmse)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="rf",RMSE = rf_rmse)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="bayesglm",RMSE = bays_rmse)

#Print RMSE's
kable_table(rmse_results,"RMSE of Various Models")

#Plot Random Forest VS Actuals
#Create Dataframes for plotting 
res_df <- data.frame(name = "rf", close = pred[,5], id = 1:nrow(test_set) )
bitcoin_cls_test <- test_set %>% dplyr::select(name,close)  
bitcoin_cls_test <- bitcoin_cls_test %>% mutate(id = 1:nrow(bitcoin_cls_test))
res_df <- rbind(res_df,bitcoin_cls_test)

#Plot predicted vs actuals
res_df %>% 
  ggplot(aes(id,close,color = name)) + geom_line() + 
  theme_economist_white() +   
  labs(title = "Random Forest: Actuals vs Predicted",
       x = "id",
       y = "Price")

#Plot predicted vs actuals
#Create Dataframes for plotting 
res_df <- data.frame(name = "gamLoess", close = pred[,4], id = 1:nrow(test_set) )
bitcoin_cls_test <- test_set %>% dplyr::select(name,close)  
bitcoin_cls_test <- bitcoin_cls_test %>% mutate(id = 1:nrow(bitcoin_cls_test))
res_df <- rbind(res_df,bitcoin_cls_test)

#Plot predicted vs actuals
res_df %>% 
  ggplot(aes(id,close,color = name)) + geom_line() + 
  theme_economist_white() +   
  labs(title = "gamLoess: Actuals vs Predicted",
       x = "id",
       y = "Price")

##################################Tune gamLoess Model##################################
set.seed(1, sample.kind="Rounding")
#Determine the best tuning parameter for degree = 2
span <- caret::train(close ~ market, method = "gamLoess",data = train_set,
                    span = data.frame(span = seq(0, 5, 0.1)))

span <- span$bestTune

kable_table(span, "Optimized Tuning Parameters")

#Optimize span and degree
cvknn_fit <- caret::train(close ~ market, method = "gamLoess",data = train_set,
                          span = 0.5, degree = 1)

y_hat <- predict(cvknn_fit,test_set,type="raw")

#Plot Optimized gamLoess VS Actuals
#Create Dataframes for plotting 
res2_df <- data.frame(name = "gamLoess", close = y_hat, id = 1:nrow(test_set) )
res2_df <- rbind(res2_df,bitcoin_cls_test)

#Determine RMSE's
gl_opt_rmse <-  RMSE(y_hat,test_set$close)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="Tuned gamLoess",RMSE = gl_opt_rmse)

#Print RMSE's
kable_table(rmse_results %>% filter(RMSE < 60),"RMSE of Tuned gamLoess Model")

#Plot predicted vs actuals
res2_df %>% 
  ggplot(aes(id,close,color = name)) + geom_line() + geom_smooth(method = 'loess') + 
  theme_economist_white() +   
  labs(title = "Tuned gamLoess Model: Act vs Pred",
       x = "id",
       y = "Price")

##################################Tune Random Forest Model##################################
set.seed(1, sample.kind="Rounding")
#Determine the best tuning parameter for 100 trees
fit <- caret::train(close ~ market, method = "rf",data = train_set, ntree=50,
             tuneGrid = data.frame(mtry = seq(1, 20, 1)))

mtry <- fit$bestTune

#Print Optimal Tunegrid parameter
kable_table(fit$bestTune,"Optimal mtry Parameter for RF Model with 100 trees")

cvknn_fit <- caret::train(close ~ market, method = "rf",data = train_set,ntree=50,
                   tuneGrid = data.frame(mtry = mtry))

y_hat <- predict(cvknn_fit,test_set,type="raw")

#Determine RMSE's
rf_opt_rmse <-  RMSE(y_hat,test_set$close)
rmse_results <- rmse_results %>% add_row(Dataset = "Train/Test", Model="Tuned rf",RMSE = rf_opt_rmse)

#Print RMSE's
kable_table(rmse_results %>% filter(Model %like% "rf"),"RMSE of Tuned Random Forest Model")

#Plot Optimized Random Forest VS Actuals
#Create Dataframes for plotting 
res3_df <- data.frame(name = "rf", close = y_hat, id = 1:nrow(test_set) )
res3_df <- rbind(res3_df,bitcoin_cls_test)


#Plot predicted vs actuals
res3_df %>% filter(id > 350) %>%
  ggplot(aes(id,close,color = name)) + geom_line() + geom_smooth(method = 'loess') +
  theme_economist_white() +   
  labs(title = "Tuned Random Forest: Act vs Pred",
       x = "id",
       y = "Price")

##################################ARIMA Model##################################
set.seed(1, sample.kind="Rounding")
#Create Time-Series Object from Bitcoin Dataset
btc_timesrs <- ts(data_bitcoin$close)

#Build Arima Model 
autoarima <- auto.arima(btc_timesrs)

#Fit data with Arima
pred <-fitted(autoarima)

#Determine Mean-Absolute-Perecentage Error (MAPE)
#Evaluate the model using MAPE

fcast_auto_arima <- predict(autoarima, n.ahead = 4)
outdata <- data_bitcoin$close[29:32]

#Determine Model Fit

out_data_pred <- ts(fcast_auto_arima$pred)
total_timeser <- ts(data_bitcoin$close)

#Plot predicted vs actuals
plot(total_timeser, col = "black", lwd = 2)
lines(c(pred, out_data_pred), col = "blue", lwd = 2)
lines(pred, col="red", lwd = 2)
title("ARIMA: Actuals Vs Predicted");

#Tune ARIMA model to include frequency and start date

count_ma = ts(data_bitcoin$close, frequency=365, start=c(2010,07))
arima = auto.arima(count_ma)
arima <- forecast(arima, h=2427)
summary(arima)

#Print Optimized Results
acc_arima <- accuracy(arima)
kable_table(acc_arima,"ARIMA Results")

rmse_results <- rmse_results %>% add_row(Dataset = "Bitcoin", Model="Auto ARIMA",RMSE = acc_arima[,2])

##############################################################################################
#Section 5: Exploring Forecasting Options
##############################################################################################
#Create Time Series Data
plot_data <- ts(data_bitcoin %>% filter (priceYear > 2017)  %>% dplyr::select(close))

#Create Simple Moving Average
sm <- ma(plot_data , order=12) # 12 month moving average
plot(sm)
title("Moving Average of Bitcoin Price");

#Plot forecasting from naive method
mn <- naive(plot_data,h=12,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(mn)
accuracy(mn)


#Plot forecasting from Exp Smoothing
plot(ses(plot_data))
accuracy(ses(plot_data))

#Plot Random Walk Drift
md <-rwf(plot_data,h=12,drift=T,level=c(90,95),fan=FALSE,lambda=NULL) 
plot(md) 
accuracy(md)

#Plot forecasting from Holtwinters
m <- HoltWinters(plot_data, gamma = FALSE)
plot(forecast(m,h=80))
accuracy(forecast(m,h=80))

#Plot forecasting from Theta Method
plot(thetaf(plot_data))
accuracy(thetaf(plot_data))

#Auto Arima
fit = auto.arima(plot_data)
tsdisplay(arima.errors(fit), main="ARIMA errors")
plot(forecast(fit))
accuracy(forecast(fit))

#Estimate Variance and Mean for next Day using Garch/ARIMA combination

#Calculate SSL series 
t<-ts(data_bitcoin[,"close"],start=1,frequency=5)
ln.t<-log(t) 
r<-diff(ln.t)
#Find a ARIMA model
fit1<-arima(r,order=c(4,0,0))
#Find a GARCH model 
res1<-resid(fit1) 
fit2<-garch(res1,order=c(2,1),trace=0)
#Forecasting 
kable_table(forecastGARCH(fit1,fit2,r=6), "Mean and Variance Forecast for next day")

#Forecasting Bitcoin price using ARIMA
#Create Time Series Data
plot_data2 <- ts(data_bitcoin %>% dplyr::select(close))

y <- ts(plot_data2, frequency=7)
z <- fourier(ts(plot_data2, frequency=365.25), K=5)
zf <- fourierf(ts(plot_data2, frequency=365.25), K=5, h=100)
fit <- auto.arima(y)
fc <- forecast(fit, h=365)
plot(fc)

##############################################################################################
#Section 6: Results
##############################################################################################
#Print Final RMSE Results
kable_table(rmse_results,"RMSE of all Models")
##############################################################################################
#Section 9: Environmental Variables
##############################################################################################
#Print OS
print("Operating System:")
version

#Print Installed Packages
print("All installed packages")
installed.packages()


