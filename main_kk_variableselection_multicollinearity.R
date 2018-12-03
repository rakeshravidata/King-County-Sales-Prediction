library(tidyverse)
library(readr)
library(leaps)

get.model.str <- function(var.in, resp.name, reg.names) {
  var.in.idx <- which(var.in)
  model.str <- paste(resp.name, "~")
  first.in <- TRUE
  for (iVAR in var.in.idx) {
    if (first.in) {
      model.str <- paste(model.str, reg.names[iVAR])
      first.in <- FALSE
    } else {
      model.str <- paste(model.str, "+", reg.names[iVAR])
    }
  }
  return(model.str)
}

eval.lm <- function(model.str, data.name) {
  lm.call.str <- paste("reg.lm <- lm(", model.str, ", data=", data.name, ")")
  eval(parse(text=lm.call.str))
  return(reg.lm)
}

forward.step <- function(curr.var.in, alpha.in, resp.name, reg.names, data.name) {
  curr.var.out.idx <- which(!curr.var.in)
  enter.idx <- NA
  if (length(curr.var.out.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=Inf, times=k)
    for (iVAR in curr.var.out.idx) {
      cand.var.in <- curr.var.in
      cand.var.in[iVAR] <- TRUE
      cand.model.str <- get.model.str(cand.var.in, resp.name, reg.names)
      cand.model.lm <- eval.lm(cand.model.str, data.name)
      iROW <- which(row.names(summary(cand.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(cand.model.lm)$coefficients[iROW,4]
    }
    enter.idx <- which.min(pval.seq)
    if (pval.seq[enter.idx] < alpha.in) {
      print(paste("Variable ", reg.names[enter.idx], " enters the model (pval=", sprintf("%6.4f", pval.seq[enter.idx]), ")", sep=""))
    } else {
      print("No variables enter the model")
      enter.idx <- NA
    }
  } else {
    print("No variables available to enter the model")
  }
  return(enter.idx)
}

backward.step <- function(curr.var.in, alpha.out, resp.name, reg.names, data.name) {
  curr.var.in.idx <- which(curr.var.in)
  leave.idx <- NA
  if (length(curr.var.in.idx) > 0) {
    k <- length(reg.names)
    pval.seq <- rep(x=-Inf, times=k)
    curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
    curr.model.lm <- eval.lm(curr.model.str, data.name)
    for (iVAR in curr.var.in.idx) {
      iROW <- which(row.names(summary(curr.model.lm)$coefficients) == reg.names[iVAR])
      pval.seq[iVAR] <- summary(curr.model.lm)$coefficients[iROW,4]
    }
    leave.idx <- which.max(pval.seq)
    if (pval.seq[leave.idx] >= alpha.out) {
      print(paste("Variable ", reg.names[leave.idx], " leaves the model (pval=", sprintf("%6.4f", pval.seq[leave.idx]), ")", sep=""))
    } else {
      print("No variables leave the model")
      leave.idx <- NA
    }
  } else {
    print("No variables available to leave the model")
  }
  return(leave.idx)
}

forward.selection <- function(alpha.in, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

backward.elimination <- function(alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=TRUE, times=k)
  stop <- FALSE
  while(!stop) {
    leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
    if (is.na(leave.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[leave.idx] <- FALSE
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}

stepwise.selection <- function(alpha.in, alpha.out, resp.name, reg.names, data.name) {
  k <- length(reg.names)
  curr.var.in <- rep(x=FALSE, times=k)
  stop <- FALSE
  while(!stop) {
    enter.idx <- forward.step(curr.var.in, alpha.in, resp.name, reg.names, data.name)
    if (is.na(enter.idx)) {
      stop <- TRUE
    } else {
      curr.var.in[enter.idx] <- TRUE
      leave.idx <- backward.step(curr.var.in, alpha.out, resp.name, reg.names, data.name)
      if (!is.na(leave.idx)) {
        curr.var.in[leave.idx] <- FALSE
        if (leave.idx == enter.idx) {
          stop <- TRUE
        }
      }
    }
  }
  curr.model.str <- get.model.str(curr.var.in, resp.name, reg.names)
  print(paste("Final model: ", curr.model.str, sep=""))
  curr.model.lm <- eval.lm(curr.model.str, data.name)
  return(curr.model.lm)
}



df<-read_csv('data/kc_house_data.csv')
df$year <- format(as.Date(df$date, format="%m/%d/%Y"),"%Y")
df$month <-format(as.Date(df$date, format="%m/%d/%Y"),"%m")
df$date = NULL
df$id<-NULL
df$lat<-NULL
df$long<-NULL

df$year<-as.factor(df$year)
df$month<-as.factor(df$month)
df$condition<-as.factor(df$condition)
df$grade<-as.factor(df$grade)
df$yr_renovated<-as.factor(df$yr_renovated)
df$yr_built<-as.factor(df$yr_built)
df$waterfront<-as.factor(df$waterfront)
df$zipcode<-as.factor(df$zipcode)
df$bedrooms<-as.factor(df$bedrooms)


test.lm<-lm(price~.,data=df)

colnames<-colnames(df)
reg.names<-colnames[2:length(colnames)]
reg.names<-c('sqft_living','sqft_basement','sqft_lot','sqft_living15','sqft_lot15')
resp.name<-'price'
data.name <- "df"
alpha.in <- 0.25
alpha.out <- 0.10


df.lm.forward <- forward.selection(alpha.in, resp.name, reg.names, data.name)

df.lm.backward <- backward.elimination(alpha.out, resp.name, reg.names, data.name)

n <- dim(df)[1]
k <- 6
p <- 7
df.vs <- regsubsets(price~ sqft_living + sqft_basement + sqft_living15 + sqft_lot15, data=df, nbest=5)
summary(df.vs)



y.bar <- mean(df$price)
y.cent <- df$price - y.bar
SS.T <- sum(y.cent^2)
y.scl <- y.cent / sqrt(SS.T)


x2.bar <- mean(df$sqft_living)
x2.cent <- df$sqft_living - x2.bar
S.22 <- sum(x2.cent^2)
x2.scl <- x2.cent / sqrt(S.22)

x3.bar <- mean(df$sqft_basement)
x3.cent <- df$sqft_basement- x3.bar
S.33 <- sum(x3.cent^2)
x3.scl <- x3.cent / sqrt(S.33)

x4.bar <- mean(df$sqft_living15)
x4.cent <- df$sqft_living15 - x4.bar
S.44 <- sum(x4.cent^2)
x4.scl <- x4.cent / sqrt(S.44)

x5.bar <- mean(df$sqft_lot15)
x5.cent <- df$sqft_lot15 - x5.bar
S.55 <- sum(x5.cent^2)
x5.scl <- x5.cent / sqrt(S.55)


X.mat.scl <- as.matrix(cbind(x2.scl, x3.scl, x4.scl, x5.scl))

XpX.mat.scl <- t(X.mat.scl) %*% X.mat.scl
Xpy.mat.scl <- t(X.mat.scl) %*% y.scl

eig.out <- eigen(XpX.mat.scl)

eig.val <- eig.out$values

eig.vect <- eig.out$vectors


t=diag(eig.vect)
k=4

#VIF Values. 
vif <- rep(x=0, times=k)
for (j in 1:k) {
  for (i in 1:k) {
    vif[j] <- vif[j] + eig.vect[j,i]^2 / eig.val[i]
  }
}
vif

#variance decomposition
var.prop <- matrix(data=0, nrow=k, ncol=k)
for (j in 1:k) {
  for (i in 1:k) {
    var.prop[i,j] <- (eig.vect[j,i]^2 / eig.val[i]) / vif[j]
  }
}
var.prop



single.val <- sqrt(eig.val)
single.val.max <- max(single.val)
cond.idx <- single.val.max / single.val
#conditional indices. 
cond.idx
