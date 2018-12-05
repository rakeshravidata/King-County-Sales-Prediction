### Model Building

# reading data into R
setwd("/Users/rakeshravi/Documents/Linear Models - R/Project/")
df <- read.csv("kc_house_data.csv", stringsAsFactors = FALSE)


#Extracting year and month from the date
df$year = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%Y"))
df$month = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%m"))
df$date = NULL
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

# Removing columns due to low correlation coefficient
df$date = NULL
df$sqft_lot = NULL
df$sqft_lot15 = NULL
df$yr_built = NULL
head(df)


df = df[df$bedrooms <= 10, ]

df$bedrooms = as.factor(df$bedrooms)


df$floors = as.factor(df$floors)

df$waterfront = as.factor(df$waterfront)


df$view= as.factor(df$view)


df$condition = as.factor(df$condition)


df$sqft_basement[df$sqft_basement != 0] = 1
# Create factor
df$sqft_basement = as.factor(df$sqft_basement)


df$yr_renovated[df$yr_renovated != 0] = 1
# Create factor
df$yr_renovated = as.factor(df$yr_renovated)



df$zipcode = as.factor(df$zipcode)


#---------------------------------------------------------------------------------------------------------------------
#Cook's Distance
cook <- cooks.distance(model)

# The associated cutoff value for identifying an         
# influential observation is                             
cut.inf <- 1

#plot to indicate outliers
plot(cook, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cook, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cook)+1, y=cook, labels=ifelse(cook>4*mean(cook, na.rm=T),names(cook),""), col="red")  # add labels

#another way of visualizing the same
plot(cooks.distance(model), pch=2, bg='orange', cex=2, ylab="Cook's distance")

#displaying influential points
df[which(cooks.distance(model) > 0.1),]

influential <- as.numeric(names(cook)[(cook > 4*mean(cook, na.rm=T))])  # influential row numbers
nrow(df[influential, ])

#removing outliers from the dataset as the number of outliers is lower than 5% of the dataset

df.after.cd = df[-influential, ]




