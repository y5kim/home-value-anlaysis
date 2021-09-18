library(dtw)
library("fUnitRoots")
library(forecast)
library(DMwR)
library(imputeTS)
library(tibbletime)
library(dplyr)
library(ggplot2)
library(maps)
library(usmap)
library(stringr)

# Load data
sale_price = read.csv("raw_median_housing_sale_price.csv")
rent_value = read.csv("not_adjusted_metro_rental_value.csv")
med_house_value = read.csv("raw_metro_house_value_med_tier.csv")
low_house_value = read.csv("metro_house_value_bottom_tier.csv")
high_house_value = read.csv("metro_house_value_top_tier.csv")
inventory = read.csv("raw_metro_forsale_inventory.csv")
med_income = read.csv("US Median income by state.csv")
med_income <- med_income[,order(ncol(med_income):1)]


# Convert to TS 
us_sale_price = ts(data=as.numeric(sale_price[1,6:dim(sale_price)[2]]), frequency=12, start=c(2008,4,30), end=c(2020,10,31))
us_rent_value = ts(data=as.numeric(rent_value[1,4:dim(rent_value)[2]]), frequency=12, start=c(2014,1,31), end=c(2020,11,31))
us_med_house_value = ts(data=as.numeric(med_house_value[1,6:dim(med_house_value)[2]]), frequency=12, start=c(1996,1,31), end=c(2020,11,30))
us_low_house_value = ts(data=as.numeric(low_house_value[1,6:dim(low_house_value)[2]]), frequency=12, start=c(1996,1,31), end=c(2020,11,30))
us_high_house_value = ts(data=as.numeric(high_house_value[1,6:dim(low_house_value)[2]]), frequency=12, start=c(1996,1,31), end=c(2020,11,30))
us_inventory = ts(data=as.numeric(inventory[1,6:dim(inventory)[2]]), frequency=12, start=c(2017,11,30), end=c(2020,11,30))
us_med_income = ts(data=as.numeric(med_income[1, 1:(ncol(med_income)-1)]), frequency=1, start=c(1984), end=c(2019))#start=c(1984,12,31), end=c(2019,12,31))


# Nationwide housing vs income line plot
yearly_house_val <- aggregate(us_med_house_value, nfrequency=1, FUN=median)
tmp = window(us_med_income, start=1996)
houseval_income <- as.data.frame(cbind(yearly_house_val, tmp))
colnames(houseval_income) <- c("house_val", "income")
houseval_income$year <- c(1996:2019)

usa_house_income_line <-  ggplot(houseval_income, aes(x=year)) +
  geom_line(aes(y=house_val), color = "navyblue") + 
  geom_line(aes(y=income*3), color="red") + 
  scale_y_continuous(
    name="House vaule",
    sec.axis=sec_axis(~./3, name="Income")
  ) +
  theme(
    axis.title.y = element_text(color = "navyblue", size=17, face="bold"),
    axis.title.y.right = element_text(color = "red", size=17, face="bold"),
    axis.text = element_text(size=17),
    axis.title.x = element_text(size = 17, face="bold"),
    plot.title = element_text(size=23, face="bold", hjust = 0.5)
  ) +
  labs(x = "Year") +
  ggtitle("National income vs house value \nfrom 1995 to 2020")

ggsave("plots/house_income_line.png", usa_house_income_line)

# Correlation between house value & income
cor(tmp, yearly_house_val)

# Nationwide housing vs rental value line plot
house <- window(us_med_house_value, start=c(2014,1))
house_rent <-cbind(house, us_rent_value)
house_rent_dat <- data.frame(as.matrix(house_rent), date=as.Date(as.yearmon(time(house_rent))))
colnames(house_rent_dat) <- c("house_val", "rent_val", 'date')

us_house_rent_line <- ggplot(house_rent_dat, aes(x=date)) +
  geom_line(aes(y=house_val), color = "navyblue") + 
  geom_line(aes(y=rent_val*130), color="red") + 
  scale_y_continuous(
    name="House value",
    sec.axis=sec_axis(~./130, name="Rental value")
  ) +
  theme(
    axis.title.y = element_text(color = "navyblue", size=17, face="bold"),
    axis.title.y.right = element_text(color = "red", size=17, face="bold"),
    axis.text = element_text(size=17),
    axis.title.x = element_text(size = 17, face="bold"),
    plot.title = element_text(size=23, face="bold", hjust = 0.5)
  ) +
  labs(x = "Date") +
  ggtitle("National house vs rental values from 2014 to 2020")

ggsave("plots/us_house_rent_line.png", us_house_rent_line)

# Correlation between house value & rent
house_rent_corr <- c()
print(cor(house, us_rent_value))  # overall
for (i in c(1:7)){
  correlation <- cor(house[((i-1)*12+1): min((i*12), length(house))], us_rent_value[((i-1)*12+1): min((i*12), length(house))])
  house_rent_corr <- c(house_rent_corr, correlation)
}

house_rent_corr <- as.data.frame(house_rent_corr); colnames(house_rent_corr) <- c("coefficient")
house_rent_corr$year <- c(2014:2020)

corr_scatter <- ggplot(house_rent_corr, aes(x=year, y=coefficient)) + geom_point(size=3.5, color="blue") +
  theme(
    axis.title.y = element_text(color = "navyblue", size=17, face="bold"),
    axis.title.y.right = element_text(color = "red", size=17, face="bold"),
    axis.text = element_text(size=17),
    axis.title.x = element_text(size = 17, face="bold"),
    plot.title = element_text(size=23, face="bold", hjust = 0.5)
  ) +
  labs(x = "Year", y = "Coefficient") +
  ggtitle("Correlation coefficient \n between house and rental values by year")

ggsave("plots/house_rent_corr.png", corr_scatter)

# Nationwide rent/income ratio
yearly_rent <- as.data.frame(aggregate(us_rent_value, nfrequency = 1, FUN = sum))
tmp <- as.data.frame(window(us_med_income, start=2014))
us_rent_income <- as.data.frame(t(rbind(tmp, yearly_rent)))
row.names(us_rent_income) <- c(2014:2019)
colnames(us_rent_income) <- c("income", "rent")
us_rent_income$income_rent_ratio <- us_rent_income$rent / us_rent_income$income
us_rent_income$year <- as.numeric(row.names(us_rent_income))


us_rent_income_bar <- ggplot(data = us_rent_income) + 
  geom_bar(data = us_rent_income, aes(x=year, y=income, fill="income"), width=0.8, stat="identity") + 
  geom_bar(data = us_rent_income, aes(x=year, y=rent, fill="rent"), width=0.6, stat="identity") + 
  geom_line(data=us_rent_income, aes(x=year, y=round(income_rent_ratio*100000), group=1), color="red") + 
  scale_y_continuous(name = "Amt ($)",  
                     sec.axis = sec_axis(~./100000, name = "Rent/Income (%)", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) +
  geom_text(aes(x=year, y=income,label = income), 
            size=5, vjust=1.6, color="navyblue") + 
  geom_text(aes(x=year, y=rent,label = rent), 
            size=5, vjust=1.6, color="navyblue") +
  theme(
    axis.title.y = element_text(size = 17,color = "navyblue", face="bold"),
    axis.title.y.right = element_text(size = 17,color = "red", face="bold"),
    axis.text = element_text(size=17),
    axis.title.x = element_text(size = 17, face="bold"),
    legend.position="bottom",
    legend.text = element_text(size=17),
    plot.title=element_text(size=23, face="bold")
  ) +
  labs(x = "Year",
    fill=" ")+
  ggtitle("National rent/income ratio by year") +
  scale_fill_manual(values = c("steelblue", "white")) 

ggsave("plots/us_rent_income_ratio_bar.png", us_rent_income_bar)
  
  
## US housing val TS model
fit_arima <- function(ts, pred_yr, buffer1=1300, buffer2=0, title="House"){
  train1 <- window(ts, end=c(pred_yr-1,12))
  test1 <- window(ts, start=c(pred_yr,1), end=c(pred_yr,12))
  arima1 <- auto.arima(train1)
  preds1 <- forecast(arima1, h=12)
  evals = ts.eval(test1, preds1$mean)
  dates = as.Date(as.yearmon(time(test1)))
  plot(dates, test1, col = "red", pch=18, type="b", xlab="Month", ylab=paste0(title, " value"), ylim=c(min(test1)-buffer2, buffer1+max(test1)))
  dates = as.Date(as.yearmon(time(preds1$mean)))
  lines(dates, preds1$mean, col="blue", pch=18, type="b")
  legend("topleft", legend=c("Forecasts", "Actual values"), col=c("blue", "red") , lty=1:2, cex=1)
  plot_name = paste0("ARIMA Forecast vs Actual ", title, " values in ", pred_yr)
  title(main=plot_name, font.main=2)
  return(list(arima1, evals))
}

house_res1 <- fit_arima(us_med_house_value, 2019)
house_res2 <- fit_arima(us_med_house_value, 2020)

rent_res1 <- fit_arima(us_rent_value, 2019, 10, 0, "Rent")
rent_res2 <- fit_arima(us_rent_value, 2020, 40, 10, "Rent")

house_evals <- as.data.frame(rbind(house_res1[[2]], house_res2[[2]]))
rent_evals <- as.data.frame(rbind(rent_res1[[2]], rent_res2[[2]]))
row.names(house_evals) <- c(2019, 2020); row.names(rent_evals) <- c(2019, 2020);

## Correlation coefficient analysis
cor(us_med_house_value, us_med_income)

## Med house value by region 
house_20 <- med_house_value[,c(3,5,(304-10):304)]
house_19 <- med_house_value[,c(3,5,(304-22):(304-11))]

avg20 <- rowSums(house_20[3:ncol(house_20)])/11
house_20 <- as.data.frame(cbind(house_20[,c(1,2)], avg20))
colnames(house_20) <- c("MSA", "state", "mean_house_val")
house_20_state <- group_by(house_20, state) %>% summarise(mean_val = mean(mean_house_val), median_val = median(mean_house_val))
house_20_state <- house_20_state[order(-house_20_state$median_val),]

house_by_state_20 <- plot_usmap(data = house_20_state, values = "mean_val", color = "red") + 
  scale_fill_continuous(name = "Mean housing value (2020)", low="white", high="red", label = scales::comma) + 
  theme(legend.position = "right", 
        plot.title = element_text(size=22, face="bold",hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text=element_text(size=13)) +
  ggtitle("Mean house value by state in 2020")

ggsave("plots/house_by_state_map.png", house_by_state_20)


house_20_sample <- house_20[order(-house_20$avg20), ]
house_20_sample <- house_20_sample[c(1:10),]
top_10_house_bar <- ggplot(data=house_20_sample, aes(x=reorder(MSA, -avg20), y=avg20, fill="streetblue")) +
  geom_bar(stat="identity") +
  theme(
    axis.text.x = element_text(angle=20, hjust=0.8, vjust=0.8, size=13, face="bold"),
    axis.text = element_text(size=17, face="bold"),
    axis.title = element_text(size=17, face="bold"),
    plot.title = element_text(size=22, face="bold"),
    legend.position = "none"
  ) + 
  xlab("Region") +
  ylab("Mean house value") +
  ggtitle("10 Regions with the highest house values") +
  geom_text(aes(x=MSA, y=avg20,label = round(avg20)), 
            position = position_stack(vjust = 0.9), size = 5, color="white", face="bold")

ggsave("plots/top_10_house_bar.png", top_10_house_bar)


rent_20_sample <- rent_20[order(-rent_20$mean_rent_val), ]
rent_20_sample <- rent_20_sample[c(1:10),]
top_10_rent_bar <- ggplot(data=rent_20_sample, aes(x=reorder(MSA, -mean_rent_val), y=mean_rent_val, fill="streetblue")) +
  geom_bar(stat="identity") +
  theme(
    axis.text.x = element_text(angle=20, hjust=0.8, vjust=0.8, size=13, face="bold"),
    axis.text = element_text(size=17, face="bold"),
    axis.title = element_text(size=17, face="bold"),
    plot.title = element_text(size=22, face="bold"),
    legend.position = "none"
  ) + 
  xlab("Region") +
  ylab("Mean rent value") +
  ggtitle("10 Regions with the highest rent values in 2020") +
  geom_text(aes(x=MSA, y=mean_rent_val,label = round(mean_rent_val)), 
            position = position_stack(vjust = 0.9), size = 5, color="white")

ggsave("plots/top_10_rent_bar.png", top_10_rent_bar)

rent_20 <- rent_value[,c(2, (ncol(rent_value)-10):ncol(rent_value))]
rent_19 <- rent_value[,c(2, (ncol(rent_value)-22):(ncol(rent_value)-11))]

avg20 <- rowSums(rent_20[2:ncol(rent_20)])/12
rent_20 <- as.data.frame(cbind(rent_20[,c(1:2)], avg20))
rent_20 <- rent_20[-1,-2]
colnames(rent_20) <- c("MSA", "mean_rent_val")

rent_20$state <- sapply(rent_20[,"MSA"], function(x) str_split(x, ", ")[[1]][2])
rent_20_state <- group_by(rent_20, state) %>% summarise(mean_val = mean(mean_rent_val), median_val = median(mean_rent_val))
rent_20_state <- rent_20_state[order(-rent_20_state$median_val),]

rent_by_state_20 <- plot_usmap(data = rent_20_state, values = "mean_val", color = "red") + 
  scale_fill_continuous(name = "Mean rent value (2020)", low="white", high="red", label = scales::comma) + 
  theme(legend.position = "right", 
        plot.title = element_text(size=22, face="bold",hjust=0.5),
        legend.title = element_text(size=13, face="bold"),
        legend.text=element_text(size=13)) +
  ggtitle("Mean rent value by state in 2020")
ggsave("plots/rent_by_state_map.png",rent_by_state_20)

##############

train1 <- window(us_med_house_value, end=c(2018,12))
test1 <- window(us_med_house_value, start=c(2019,1), end=c(2019,12))
arima1 <- auto.arima(train1)

train2 <- window(us_rent_value, end=c(2018,12))
test2 <- window(us_rent_value, start=c(2019,1), end=c(2019,12))
arima2 <- auto.arima(train2)

# Decomposition of time series
print(ndiffs(train1))
ts_comp1 = decompose(train1)
plot(ts_comp1)
# Note the unexplainable part around 2008; upward trend and seasonality

print(ndiffs(train2))
ts_comp2 = decompose(train2)
plot(ts_comp2)

# Data preprocessing
#train_adj1 <- train1- ts_comp1$seasonal
Stationary <- diff(train1, differences=2)
plot(Stationary, xlab="Year", ylab="Series") 
title("Twice differenced home value series")
acf(Stationary)
pacf(Stationary)
# Need to add seasonal coefficient!

Stationary2 <- diff(train2, differences=1)
plot(Stationary2, xlab="Year", ylab="Series")
title("Differenced rent value series")
acf(Stationary2)
pacf(Stationary2)

# Unit root test
urkpssTest(train1, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)

# Predictions 
preds1 <- forecast(arima1, h=12)
plot(preds1$mean, col = "blue", pch=18, type="b", xlab="Date", ylab="House value")
lines(test1, col="red", pch=18, type="b")
legend( "topleft", legend=c("Forecasts", "Actual values"), col=c("blue", "red") , lty=1:2, cex=1)

evals = ts.eval(test1, preds1$mean)
evals[["test_start_yr"]] = start(test_ts)[1]
print(evals["mae"])

plot(preds1$mean, col = "blue", pch=18, type="b", xlab="Date", ylab="House value")
lines(test1, col="red", pch=18, type="b")
legend( "topleft", legend=c("Forecasts", "Actual values"), col=c("blue", "red") , lty=1:2, cex=1)

