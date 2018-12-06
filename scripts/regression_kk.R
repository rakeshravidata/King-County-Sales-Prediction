library(dplyr)
library(readr)
# Import data
df <- read_csv("data/kc_house_data.csv")

colnames(df)

numberOfNA = length(which(is.na(df) == T))
if(numberOfNA > 0)
{
  cat('Number of missing values: ', numberOfNA)
  cat('\nRemoving missing values...')
  df = df[complete.cases(df), ]
}

df$year <- format(as.Date(df$date, format="%m/%d/%Y"),"%Y")
df$month <-format(as.Date(df$date, format="%m/%d/%Y"),"%m")

#original model with all variables and no transformations
model.orig<-lm(price~.,data=df)
summary(model.orig)
#adjusted R^2 of 0.7023

#-------------MODEL ADEQUACY CHECKS---------------------#

price <- df$price
price.fit <- fitted(model.orig)
SS.Res <- sum((price - price.fit)^2)
SS.R <- sum((price.fit - mean(price))^2)
n <- nrow(df)
k <- ncol(df) - 1
F <- (SS.R / 10) / (SS.Res / (n - k - 1))
# F statistics
F
# [1] 5222.791
pf <- pf(abs(F), df1=k, df2=n-k-1, lower.tail=FALSE)
# P-value of the F statistics
pf
# [1] 0
# The model is significant

# Test on genenral linear hypothesis
MS.Res <- SS.Res / (n - k - 1)
colnames <- colnames(df)[-1]

# Generate Partial F-test on each of the variables
results <- vector("list", length(colnames))
names(results) <- colnames

for(i in 1:k){
  var <- colnames[i]
  data.lm.pr <- lm(price~., select(df, -var))
  price.fit.pr <- fitted(model.orig)
  SS.R.pr <- sum((price.fit.pr - mean(price))^2)
  F0 <- (SS.R - SS.R.pr) / MS.Res
  pf <- pf(abs(F0), df1=1, df2=n-k-1, lower.tail=FALSE)
  result <- c(F0, pf)
  names(result) <- c("F0", "p-value")
  results[[var]] <- result
}
results

# Check the model adequecy
# Residuals
e <- resid(model.orig)
# Standardized Residuals
d <- e / sqrt(MS.Res)
# Studentized residuals
h.val<-hatvalues(model.orig)
r <- e / sqrt(MS.Res * (1 - h.val))

# Press residuals
e.press <- e / (1 - h.val)
# R-student residuals
p <- k + 1
Si.sq <- ((n-p) * MS.Res - e^2/(1-h.val)) / (n-p-1)
t <- e / sqrt(Si.sq * (1 - h.val))
# Create a dataframe of all kinds of residuals
data.resids <- data.frame(e, d, e.press, t)
data.resids[1:10,]

# Plot the Normal Probability
qqnorm(data.resids$t, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", 
       main = "Normal probability plot of residuals")
# The normal probablility plot is skewed

# Plot the residuals against the fitted values
plot(price.fit, data.resids$t, pch=20, cex=1, xlab="fitted value", 
     ylab="R-student residual", main = "Residual vs. fitted value plot")
lines(c(min(price.fit), max(price.fit)), c(0,0), type="l", lty=1, lwd=3)
# The residuals exhibit patterns and outliers

#performing transformations, removing variables and ordinalization

df$price =log(df$price)
df$date = NULL
df$id<-NULL
df$lat<-NULL
df$long<-NULL
df$sqft_living<-NULL

df$year<-as.factor(df$year)
df$month<-as.factor(df$month)
df$condition<-as.factor(df$condition)
df$grade<-as.factor(df$grade)
df$yr_renovated<-as.factor(df$yr_renovated)
df$yr_built<-as.factor(df$yr_built)
df$waterfront<-as.factor(df$waterfront)
df$zipcode<-as.factor(df$zipcode)
df$bedrooms<-as.factor(df$bedrooms)

#model after doing transformations, removing variables and converting them
data.lm <- lm(price~., df)

summary(data.lm)
# R-sq = 0.8862

price.fit <- fitted(data.lm)
SS.Res <- sum((price - price.fit)^2)
SS.R <- sum((price.fit - mean(price))^2)
n <- nrow(df)
k <- ncol(df) - 1
F <- (SS.R / 10) / (SS.Res / (n - k - 1))
# F statistics
F
# [1] 1475.468
pf <- pf(abs(F), df1=k, df2=n-k-1, lower.tail=FALSE)
# P-value of the F statistic
pf
# [1] 0
# The model is significant


#Using cook's d to check for influence points and removing them.
cook <- cooks.distance(data.lm)

# The associated cutoff value for identifying an         
# influential observation is                             
cut.inf <- 1

#displaying influential points
df[which(cooks.distance(data.lm) > 0.1),]

influential <- as.numeric(names(cook)[(cook > 4*mean(cook, na.rm=T))])  # influential row numbers
nrow(df[influential, ])

plot(cook, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cook, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cook)+1, y=cook, labels=ifelse(cook>4*mean(cook, na.rm=T),names(cook),""), col="red")  # add labels


#removing outliers from the dataset as the number of outliers is lower than 5% of the dataset

df.after.cd = df[-influential[!is.na(influential)], ]

#Create a new, final model with cleaned dataset
data.lm.after.cd<-lm(log(price)~.,data=df.after.cd)

summary(data.lm.after.cd)

#R^2 of 0.9063

# Test the significance of regression
price <- df.after.cd$price
price.fit <- fitted(data.lm.after.cd)
SS.Res <- sum((price - price.fit)^2)
SS.R <- sum((price.fit - mean(price))^2)
n <- nrow(df.after.cd)
k <- ncol(df.after.cd) - 1
F <- (SS.R / 10) / (SS.Res / (n - k - 1))
# F statistics
F
# [1]F value of 2079.985, there is significance of regression

pf <- pf(abs(F), df1=k, df2=n-k-1, lower.tail=FALSE)
# P-value of the F statistics
pf
# [1] 0
# The model is significant

# Test on genenral linear hypothesis
MS.Res <- SS.Res / (n - k - 1)
colnames <- colnames(df.after.cd)[-1]

# Generate Partial F-test on each of the variables
results <- vector("list", length(colnames))
names(results) <- colnames

for(i in 1:k){
  var <- colnames[i]
  data.lm.pr <- lm(price~., select(df.after.cd, -var))
  price.fit.pr <- fitted(data.lm.pr)
  SS.R.pr <- sum((price.fit.pr - mean(price))^2)
  F0 <- (SS.R - SS.R.pr) / MS.Res
  pf <- pf(abs(F0), df1=1, df2=n-k-1, lower.tail=FALSE)
  result <- c(F0, pf)
  names(result) <- c("F0", "p-value")
  results[[var]] <- result
}
results
# $`bedrooms`
# F0  p-value 
# 20745.35     0.00 
# 
# $bathrooms
# F0  p-value 
# 20745.34     0.00 
# 
# $sqft_lot
# F0  p-value 
# 20745.37     0.00 
# 
# $floors
# F0  p-value 
# 20745.38     0.00 
# 
# $waterfront
# F0  p-value 
# 20745.55     0.00 
# 
# $view
# F0  p-value 
# 20745.56     0.00 
# 
# $condition
# F0  p-value 
# 20745.49     0.00 
# 
# $grade
# F0  p-value 
# 20745.86     0.00 
# 
# $sqft_above
# F0  p-value 
# 20746.28     0.00 
# 
# $sqft_basement
# F0  p-value 
# 20745.61     0.00 
# 
# $yr_built
# F0  p-value 
# 20745.53     0.00 
# 
# $yr_renovated
# F0  p-value 
# 20745.37     0.00 
# 
# $zipcode
# F0  p-value 
# 20756.16     0.00 
# 
# $sqft_living15
# F0  p-value 
# 20745.43     0.00 
# 
# $sqft_lot15
# F0 p-value 
# 20745.3     0.0 
# 
# $year
# F0  p-value 
# 20745.34     0.00 
# 
# $month
# F0  p-value 
# 20745.35     0.00 
# 
# > results
# $`bedrooms`
# F0  p-value 
# 20745.35     0.00 
# 
# $bathrooms
# F0  p-value 
# 20745.34     0.00 
# 
# $sqft_lot
# F0  p-value 
# 20745.37     0.00 
# 
# $floors
# F0  p-value 
# 20745.38     0.00 
# 
# $waterfront
# F0  p-value 
# 20745.55     0.00 
# 
# $view
# F0  p-value 
# 20745.56     0.00 
# 
# $condition
# F0  p-value 
# 20745.49     0.00 
# 
# $grade
# F0  p-value 
# 20745.86     0.00 
# 
# $sqft_above
# F0  p-value 
# 20746.28     0.00 
# 
# $sqft_basement
# F0  p-value 
# 20745.61     0.00 
# 
# $yr_built
# F0  p-value 
# 20745.53     0.00 
# 
# $yr_renovated
# F0  p-value 
# 20745.37     0.00 
# 
# $zipcode
# F0  p-value 
# 20756.16     0.00 
# 
# $sqft_living15
# F0  p-value 
# 20745.43     0.00 
# 
# $sqft_lot15
# F0 p-value 
# 20745.3     0.0 
# 
# $year
# F0  p-value 
# 20745.34     0.00 
# 
# $month
# F0  p-value 
# 20745.35     0.00 

# All of the variables passed the partial F-test

# Check the model adequecy
# Residuals
e <- resid(data.lm.after.cd)
# Standardized Residuals
d <- e / sqrt(MS.Res)
# Studentized residuals
r <- e / sqrt(MS.Res * (1 - h.val))
h.val<-hatvalues(data.lm.after.cd)
# Press residuals
e.press <- e / (1 - h.val)
# R-student residuals
p <- k + 1
Si.sq <- ((n-p) * MS.Res - e^2/(1-h.val)) / (n-p-1)
t <- e / sqrt(Si.sq * (1 - h.val))
# Create a dataframe of all kinds of residuals
data.resids <- data.frame(e, d, e.press, t)
data.resids[1:10,]

# Plot the Normal Probability
qqnorm(data.resids$t, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", 
       main = "Normal probability plot of residuals")
# The normal probablility plot is a straight line

# Plot the residuals against the fitted values
plot(price.fit, data.resids$t, pch=20, cex=1, xlab="fitted value", 
     ylab="R-student residual", main = "Residual vs. fitted value plot")
lines(c(min(price.fit), max(price.fit)), c(0,0), type="l", lty=1, lwd=3)
# The residuals do not show any normality problems

