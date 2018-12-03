# Cleaning (Just the Cleaning piece of the Exploration/Cleaning File)

setwd("~/UVA/DSI/Fall 2018/STAT 6021/project/kc-housesales-data")
df <- read.csv('kc_house_data.csv')


# Extracting Date Features ==========
df$year = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%Y"))
df$month = as.numeric(format(as.Date(df$date, format="%m/%d/%Y"),"%m"))

# Response ==========
df$price =log(df$price)

# Factor Variables ==========
df$bedrooms = as.factor(df$bedrooms)

df$floors = as.factor(df$floors)

df$waterfront = as.factor(df$waterfront)

df$view= as.factor(df$view)

df$condition = as.factor(df$condition)

df$sqft_basement[df$sqft_basement != 0] = 1
df$sqft_basement = as.factor(df$sqft_basement)

df$yr_renovated[df$yr_renovated != 0] = 1
df$yr_renovated = as.factor(df$yr_renovated)

df$zipcode = as.factor(df$zipcode)

# Remove Columns ========
df$date = NULL
df$id = NULL
df$sqft_lot = NULL
df$sqft_lot15 = NULL
df$yr_built = NULL
