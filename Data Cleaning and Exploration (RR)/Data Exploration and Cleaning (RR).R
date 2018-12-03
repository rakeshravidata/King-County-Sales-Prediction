### Model Building

# reading data into R
setwd("/Users/rakeshravi/Documents/Linear Models - R/Project/")
df <- read.csv("kc_house_data.csv", stringsAsFactors = FALSE)

# Splitting into test and training samples
set.seed(999)
index<-sample(nrow(df),0.70*nrow(df),replace=F)
train<-df[index,]
test<-df[-index,]

#running a basic model;
modview <- lm(price~.,data=train)
summary(modview)

#model contains a number of levels of dates
#split date into year and month
#The number of levels in Date are too many as house prices generally vary from month to month, we can use just the month and
# the year component from the date

#Extracting year and month from the date
df$year = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%Y"))
df$month = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%m"))
df$date = NULL

#running a basic model once again
modview <- lm(price~.,data=df)
summary(modview)

#the number of levels derived from month and year is manageble

#check the distribution of prices (response variables)
hist(df$price)
#the result is heavily skewed and manjority of the data points lie near the origin. Log transformation might help

df$price =log(df$price)
hist(df$price)
#the distribution looks more smooth now

#removing missing values from data
# Check for NA and missing values
# is.na return a vector with value T for missing values.
numberOfNA = length(which(is.na(df) == T))
if(numberOfNA > 0)
{
  cat('Number of missing values: ', numberOfNA)
  cat('\nRemoving missing values...')
  df = df[complete.cases(df), ]
}

# Removing columns id as we do not need it
df$id = NULL

#Correlation plots
library(pacman)
pacman:: p_load(Metrics, car, corrplot, caTools, ggplot2, DAAG)
df$date = NULL
corr = cor(df[, 1:20])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

#There are lots of correlations between variables that have over 0.6 as their coefficients

#correlations between response variable and the regressors
# Correlation between price and sqft_living is (0.7)
# Correlation between price and grade is (0.67)
# Correlation between price and sqft_above is (0.61)
# Correlation between price and sqft_living15 is (0.59)
# Correlation between price and bathrooms is (0.53)

## date, sqft_lot, yr_built, long, sqft_lot15  happen to have the lowest correlations with the response variable (less than 0.1)

#feature-feature correlation (Potentially multicollinear)
# Correlation between sqft_living and sqft_above is (0.88)
# Correlation between sqft_living and sqft_living15 is (0.76)
# Correlation between sqft_living and grade is (0.76)
# Correlation between sqft living and bathrooms is (0.75)

# Removing columns
df$date = NULL
df$sqft_lot = NULL
df$sqft_lot15 = NULL
df$yr_built = NULL
head(df)

#Box plots
# Boxplot between price and bedrooms
boxplot(df[, 1] ~ df[, 2], main = 'Price vs Bedrooms', col=c("blue","red"))
#bedrooms does not seem to have a linear relationship with price
#for 11 and 33 number of bedrooms, there are hardly any houses.

#Lets examine a subset of the houses that has more than 10 bedrooms
print(subset(df, df$bedrooms > 10))

#there are only two houses and it looks like they do not have nearly enough bathrooms for this house to 
#really exist in real life. These looks obvious data entry error and will be removed from the dataset.
df = df[df$bedrooms <= 10, ]

#because there are only ten levels of bedrooms, it makes sense to convert into a factor without increasing the 
#dimensionality of the dataset.
df$bedrooms = as.factor(df$bedrooms)

# Boxplot between price and bathrooms
boxplot(df[, 1] ~ df[, 3], main = 'Price vs Bathrooms', col=c("blue","red"))
#there seems to be a linear relationship betweem Price and Bathrooms

# Boxplot between price and sqft_living
boxplot(df[, 1] ~ df[, 4], main = 'Price vs Sqft_living', col=c("blue","red"))
#there is a pseudo-linear relationship

# Boxplot between price and floors
boxplot(df[, 1] ~ df[, 5], main = 'Price vs floors', col=c("blue","red"))

#Floors dont show a pattern and are clearly categorical so converting it to a factor

df$floors = as.factor(df$floors)

# Boxplot between price and waterfront
boxplot(df[, 1] ~ df[, 6], main = 'Price vs waterfront', col=c("blue","red"))
# Waterfront are clearly a categorical value. 
# Converting it into factor
df$waterfront = as.factor(df$waterfront)

# Boxplot between price and view
boxplot(df[, 1] ~ df[, 7], main = 'Price vs View', col=c("blue","red"))
# View is a categorical value. 
# Converting it into factor
df$view= as.factor(df$view)

# Boxplot between price and condition
boxplot(df[, 1] ~ df[, 8], main = 'Price vs Condition', col=c("blue","red"))
# Condition is clearly a categorical value. 
# Converting it into factor
df$condition = as.factor(df$condition)

# Boxplot between price and grade
boxplot(df[, 1] ~ df[, 9], main = 'Price vs Grade', col=c("blue","red"))
#there is a linear relationship

# Boxplot between price and sqft_basement
boxplot(df[, 1] ~ df[, 10], main = 'Price vs Sqft_basement', col=c("blue","red"))
#there is a linear relationship


# Checking the number of houses that do not have a basement
length(df$sqft_basement[df$sqft_basement == 0])
# 13110 houses do not have basement. 
# This can also be converted into a factor with either having a basement or not having a basement
df$sqft_basement[df$sqft_basement != 0] = 1
# Create factor
df$sqft_basement = as.factor(df$sqft_basement)

# Boxplot between price and yr_renovated
boxplot(df[, 1] ~ df[, 11], main = 'Price vs yr_renovated', col=c("blue","red"))
#Lets check how many rows do not have a yr_renovated
length(df$yr_renovated[df$yr_renovated == 0])
#Over 90% of the houses have not been renovated and we can just convert the years to one to convert this into 
#a factor of a smaller number of levels
df$yr_renovated[df$yr_renovated != 0] = 1
# Create factor
df$yr_renovated = as.factor(df$yr_renovated)


# Boxplot between price and zipcode
boxplot(df[, 1] ~ df[, 12], main = 'Price vs zipcode', col=c("blue","red"))
# Condition is clearly a categorical value. 
# Converting it into factor
df$zipcode = as.factor(df$zipcode)

#boxplot between price and sqft_kliving15
boxplot(df[, 1] ~ df[, 16], main = 'Price vs sqft_living15', col=c("blue","red"))
#there is indication of a possible linear relationship

# Plot for Price
plot(df[, 3], df[, 1], main = 'Price vs Sqft_living')
# there is a house at the bottom right with a really high value of sqft but low price which seems a bit odd


# running a basic model to set benchmark for future improvments on the linear models
#-----------------------------------------------------------------------------------------------------------------------

# Splitting dataset into training set and test set
set.seed(123) # Seed initializes the randomness
sample = sample.split(df, SplitRatio = 0.7) # Returns a vector with T for 70% of data
trainingSet = subset(df, sample == T)
testSet = subset(df, sample == F)

# Create model 
model = lm(formula = price ~  bedrooms + bathrooms + floors + waterfront + view + condition +
             + sqft_basement + yr_renovated + zipcode + sqft_living15 + sqft_living + grade,
           data = trainingSet)
summary(model)

# Bedrooms are not statisticaaly significant, so we would omit it.
model = lm(formula = price ~  bathrooms + floors + waterfront + view + condition +
             + sqft_basement + yr_renovated + zipcode + sqft_living15 + sqft_living + grade,
           data = trainingSet)
summary(model)

#making predictions
y_pred = predict(model, newdata = trainingSet)


# Visualizing the training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = trainingSet$sqft_living, y = trainingSet$price),
             colour = 'red') +
  geom_line(aes(x = trainingSet$sqft_living, y = y_pred),
            colour = 'blue') +
  ggtitle('Sqft_living vs Price (Training set)') +
  xlab('Sqft_living') +
  ylab('Price')

# Visualizing the test set results
ggplot() +
  geom_point(aes(x = testSet$sqft_living, y = testSet$price),
             colour = 'red') +
  geom_line(aes(x = trainingSet$sqft_living, y = y_pred),
            colour = 'blue') +
  ggtitle('Sqft_living vs Price (Test set)') +
  xlab('Sqft_living') +
  ylab('Price')

# Checking accuracy on test set
pricePrediction = predict(model, newdata = testSet)
modelOutput <- cbind(testSet, pricePrediction)

#Test with RMSE
library(hydroGOF)
rmse(modelOutput$price, modelOutput$pricePrediction)

