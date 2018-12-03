
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
boxcox(price ~ .-id, data=house.data, lambda=seq(from=0, to=2, by=0.01))
?boxcox

hist(house.data$price ,xlab = "Price",col = "yellow", xlim = c(0,3000000), breaks = 500)

?hist

ridge.reg.coefficients <- function(y.vect, X0.mat, plot=TRUE, grid.size=25, grid.st=0.001, grid.fn=0.5) {
  # Collect parameters
  n <- dim(X0.mat)[1]
  k <- dim(X0.mat)[2]
  p <- k + 1
  # Unit-length scaling
  y.bar <- mean(y.vect)
  y.cent <- y.vect - y.bar
  SS.T <- sum(y.cent^2)
  y.vect.scl <- y.vect / sqrt(SS.T)
  X.mat.scl <- matrix(data=NA, nrow=n, ncol=k)
  x.bar.list <- numeric(length=k)
  css.list <- numeric(length=k)
  for (j in 1:k) {
    x.bar.list[j] <- mean(X0.mat[,j])
    xj.cent <- X0.mat[,j] - x.bar.list[j]
    css.list[j] <- sum(xj.cent^2)
    X.mat.scl[,j] <- xj.cent / sqrt(css.list[j])
  }
  # Calculate ridge trace diagram
  ridge.k.grid <- exp(seq(from=log(grid.st), to=log(grid.fn), length.out=grid.size))
  b.hat.R.scl.list <- matrix(data=0, nrow=k, ncol=grid.size)
  y.vect.aug <- rbind(y.vect.scl, matrix(data=0, nrow=k, ncol=1))
  for (iVAL in 1:grid.size) {
    ridge.k.val <- ridge.k.grid[iVAL]
    X.mat.aug <- rbind(X.mat.scl, sqrt(ridge.k.val)*diag(k))
    XpX.mat.aug <- t(X.mat.aug) %*% X.mat.aug
    Xpy.mat.aug <- t(X.mat.aug) %*% y.vect.aug
    XpX.inv.aug <- solve(XpX.mat.aug)
    b.hat.R.scl.list[,iVAL] <- XpX.inv.aug %*% Xpy.mat.aug
  }
  if (plot) {
    plot(ridge.k.grid, rep(x=0, times=grid.size), pch=3, cex=1, ylim=c(min(b.hat.R.scl.list), max(b.hat.R.scl.list)), xlab="ridge constant, k", ylab="fitted ridge regression coefficient", main = "Ridge trace diagram")
    abline(h=0, lty=1, lwd=1)
    for (j in 1:k) {
      lines(ridge.k.grid, b.hat.R.scl.list[j,], type="l", lty=1, lwd=3)
    }
  }
  # Convert to the original scale and calculate MS.Res and R2.
  X.mat <- as.matrix(cbind(rep(x=1, times=n), X0.mat))
  b.hat.R.list <- matrix(data=0, nrow=p, ncol=grid.size)
  SS.Res.list <- numeric(length=grid.size)
  R2.list <- numeric(length=grid.size)
  for (iVAL in 1:grid.size) {
    b.hat.R.list[1,iVAL] <- y.bar
    for (j in 1:k) {
      b.hat.R.list[j+1,iVAL] <- b.hat.R.scl.list[j,iVAL] / sqrt(css.list[j] / SS.T)
      b.hat.R.list[1,iVAL] <- b.hat.R.list[1,iVAL] - b.hat.R.list[j+1,iVAL]*x.bar.list[j]
    }
    SS.Res.list[iVAL] <- sum((y.vect - X.mat %*% b.hat.R.list[,iVAL])^2)
    R2.list[iVAL] <- 1 - SS.Res.list[iVAL] / SS.T
  }
  MS.Res.list <- SS.Res.list / (n-p)
  out.list <- list(ridge.k.grid=ridge.k.grid, b.hat.R.list=b.hat.R.list, MS.Res.list=MS.Res.list, R2.list=R2.list)
  return(out.list)
}


# Using the df from  Data Cleaning =====


ridge.reg.coefficients(df$price, as.matrix(df[,-1]))
# this is giving me an error I don't remember exactly how I resolved when we did the homework
# try onehot encoding each of the categorical variables that we can (waterfront, view, sqft_basement, yr_renovated)
# might suggest removing the time stuff for the ridge regression

# which(is.na(df)) # Nothing is missing!
