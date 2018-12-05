
# Group 42
# Housing Market: Unit 6 Material

setwd("~/UVA/DSI/Fall 2018/STAT 6021/project/kc-housesales-data")

house.data <- read.csv('kc_house_data.csv')

library(car)

# ?boxTidwell()

# it doesn't makes sense to transform regressors, given the structure of our data is mostly categorical

# house.data <= 0
# house.data$long = abs(house.data$long) # convert this so that we have all positive for box-tidwell
# # update categorical variables so that they are one or more
# house.data$waterfront = as.factor(as.numeric(house.data$waterfront) + 1)
# house.data$view = as.factor(as.numeric(house.data$view) + 1)
# house.data$sqft_basement = as.factor(as.numeric(house.data$sqft_basement) + 1)
# house.data$yr_renovated = as.factor(as.numeric(house.data$yr_renovated) + 1)
# 
# boxTidwell(price ~ .-date -id, data=house.data) # maximizing alpha is the same as lambda using the car library


library(MASS)
house.data$date = as.Date(house.data$date, format = "%m/%d/%Y")
boxcox(price ~ .-id, data=house.data, lambda=seq(from=0, to=.2, by=0.01)) # before actual cleaning
boxcox(price ~ ., data=df, lambda=seq(from=0, to=.2, by=0.01)) # cleaning before pricing logged


# ?boxcox

hist(house.data$price ,xlab = "Price",col = "yellow", xlim = c(0,10000000), breaks = 100, main= "Frequency of House Prices \n(Unadjusted)")
hist(log(house.data$price) ,xlab = "Price",col = "yellow", xlim = c(10,17), breaks = 50, main= "Frequency of House Prices \n(Log Transform)")

df$price = log(df$price)

# ?hist

# removed ridge regression
