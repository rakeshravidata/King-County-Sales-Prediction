library(dplyr)

# Import data
data <- read.csv("kc_house_data.csv")

# Select useful columns
data.price <- select(data, c("price","bedrooms","bathrooms","sqft_lot","floors","condition",
                            "grade","sqft_above","sqft_basement"))
age <- 2018 - data$yr_built
data.price <- cbind(data.price, age)

# Fit a linear regression model 
data.lm <- lm(price~., data.price)
summary(data.lm)$coefficients
#                    Estimate   Std. Error    t value      Pr(>|t|)
# (Intercept)   -1.088274e+06 1.813254e+04 -60.017770  0.000000e+00
# bedrooms      -4.903076e+04 2.122144e+03 -23.104353 1.089238e-116
# bathrooms      4.968293e+04 3.647753e+03  13.620145  4.537069e-42
# sqft_lot      -2.263292e-01 3.840056e-02  -5.893903  3.827861e-09
# floors         2.876962e+04 3.921924e+03   7.335588  2.285474e-13
# condition      1.882481e+04 2.587990e+03   7.273912  3.611502e-13
# grade          1.326334e+05 2.259631e+03  58.696901  0.000000e+00
# sqft_above     1.810695e+02 3.669622e+00  49.342827  0.000000e+00
# sqft_basement  2.029833e+02 4.737508e+00  42.845995  0.000000e+00
# age            3.964099e+03 6.982940e+01  56.768334  0.000000e+00

# Test the significance of regression
price <- data.price$price
price.fit <- fitted(data.lm)
SS.Res <- sum((price - price.fit)^2)
SS.R <- sum((price.fit - mean(price))^2)
n <- nrow(data.price)
k <- ncol(data.price) - 1
F <- (SS.R / 10) / (SS.Res / (n - k - 1))
# F statistics
F
# [1] 3496.523
pf <- pf(abs(F), df1=k, df2=n-k-1, lower.tail=FALSE)
# P-value of the F statistics
pf
# [1] 0
# The model is siginifant

# Test on genenral linear hypothesis
MS.Res <- SS.Res / (n - k - 1)
colnames <- colnames(data.price)[-1]

# Generate Partial F-test on each of the variables
results <- vector("list", length(colnames))
names(results) <- colnames

for(i in 1:k){
  var <- colnames[i]
  data.lm.pr <- lm(price~., select(data.price, -var))
  price.fit.pr <- fitted(data.lm.pr)
  SS.R.pr <- sum((price.fit.pr - mean(price))^2)
  F0 <- (SS.R - SS.R.pr) / MS.Res
  pf <- pf(abs(F0), df1=1, df2=n-k-1, lower.tail=FALSE)
  result <- c(F0, pf)
  names(result) <- c("F0", "p-value")
  results[[var]] <- result
}

# The results are the F statistics and p-values for the partial F-test on each variable
results 
# $`bedrooms`
#           F0       p-value 
# 5.338111e+02 1.089238e-116 

# $bathrooms
#           F0      p-value 
# 1.855083e+02 4.537069e-42 

# $sqft_lot
#           F0      p-value 
# 3.473809e+01 3.827861e-09 

# $floors
#           F0      p-value 
# 5.381085e+01 2.285474e-13 

# $condition
#           F0      p-value 
# 5.290980e+01 3.611502e-13 

# $grade
#       F0  p-value 
# 3445.326    0.000 

# $sqft_above
#       F0  p-value 
# 2434.715    0.000 

# $sqft_basement
#       F0  p-value 
# 1835.779    0.000 

# $age
#       F0  p-value 
# 3222.644    0.000 
# All of the variables passed the partial F-test

# Test the overall adequecy
SS.T <- sum((price - mean(price))^2)
R.sq <- 1 - SS.Res / SS.T
R.sq
# [1] 0.6182821
R.sq.adj <- 1 - (SS.Res / (n-k-1)) / (SS.T / (n-1))
R.sq.adj
# [1] 0.618123

# Logrithm transform price and fit the model
log.lm <- lm(log(price)~., data.price)
price.fit <- fitted(log.lm)
SS.Res <- sum((price - exp(price.fit))^2)
R.sq <- 1 - SS.Res / SS.T
R.sq
# [1] 0.4946665
# Its R square is lower than the normal model without logrithm transform on price


# Check the model adequecy
# Residuals
e <- resid(data.lm)
# Standardized Residuals
d <- e / sqrt(MS.Res)
# Studentized residuals
X.mat <- as.matrix(select(data.price, -price))
X.mat <- cbind(rep(1,n), X.mat)
H.mat <- X.mat %*% solve(t(X.mat) %*% X.mat) %*% t(X.mat)
h.val <- diag(H.mat)
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
#             e           d     e.press           t
# 1   -68215.30 -0.30048206  -68230.060 -0.30050823
# 2  -118936.78 -0.52390543 -118982.241 -0.52399675
# 3   -38499.86 -0.16958829  -38510.437 -0.16960777
# 4   104218.12  0.45907111  104287.097  0.45921461
# 5    74434.81  0.32787839   74453.318  0.32791238
# 6  -312631.43 -1.37711237 -313055.719 -1.37807521
# 7   -61532.23 -0.27104377  -61543.430 -0.27106261
# 8    31253.49  0.13766870   31259.287  0.13767835
# 9  -165021.64 -0.72690496 -165079.304 -0.72702401
# 10   -8485.96 -0.03737986   -8487.968 -0.03738342

# Plot the Normal Probability
qqnorm(data.resids$t, datax=TRUE, pch=16, cex=1, xlab="percent", ylab="R-student residual", 
       main = "Normal probability plot of residuals")
# The graph demostrate a negative skew

# Plot the residuals against the fitted values
plot(price.fit, data.resids$t, pch=20, cex=1, xlab="fitted value", 
     ylab="R-student residual", main = "Residual-by-fitted-value plot")
lines(c(min(price.fit), max(price.fit)), c(0,0), type="l", lty=1, lwd=3)
# The residuals do not show any normality problem

# Plot the residuals by regressors
par(mfrow=c(3,3))
for(i in 1:k){
  var <- colnames[i]
  plot(data.price[,var], data.resids$t, pch=20, cex=1, xlab=var,
       ylab="R-student residual", main = paste("Residual by", var, "plot"))
}

# The final linear regression model
coefficients(data.lm)
#   (Intercept)      bedrooms     bathrooms      sqft_lot        floors     condition         grade    sqft_above 
# -1.088274e+06 -4.903076e+04  4.968293e+04 -2.263292e-01  2.876962e+04  1.882481e+04  1.326334e+05  1.810695e+02 
# sqft_basement           age 
#  2.029833e+02  3.964099e+03 
