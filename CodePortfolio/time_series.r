# Time series Analaysis - Spring 2019
# Code Portfolio
# Pradeesh Unnikrishnan

#Introduction to R
#Introduction to R - https://www.datacamp.com/courses/free-introduction-to-r 

#Code from the lecture
#Create Matrix 3 [row] x 1 [column]

a <- matrix(c(.85, .75, .65), nrow=3, ncol = 1, byrow=TRUE)

#check dimension of matrix
dim(a)

#convert data dataframe into matrix
x <- as.matrix(data.frame(colum1 = c(.85, .75, .65), column2 = c(1.5,1,.5)))

#Matrix algebra

#addition
# 3 x 2
x <- cbind(c(.85, .75, .65), c(1.5,1,.5))
# 3 x 2
y <- cbind(c(1, 2, 1), c(.5,.6,.5))
x

#scalar multiplication
t <- matrix(c(1.5, 1, .5), nrow=3, ncol=1, byrow = TRUE)
c = 2
t * 2


#Matrix multiplication
# 3 x 2
x <- cbind(c(.85, .75, .65), c(1.5,1,.5))
# 3 x 2
y <- rbind(c(1, 2, 1), c(.5,.6,.5))

x %*% y

#Matrix transpose

z <- t(x)
dim(z)


#Metdata of matrix
nrow(x)
## [1] 3
ncol(x)
## [1] 2
dim(x)
## [1] 3 2
colSums(x)
## [1] 2.25 3.00
rowSums(x)
## [1] 2.35 1.75 1.15
sum(x)
## [1] 5.25
colMeans(x)
## [1] 0.75 1.00
rowMeans(x)
## [1] 1.175 0.875 0.575
mean(x)
## [1] 0.875


sum(x) #Sum of the data

length(x) #length of the list

mean(x) # Mean or average

var(x) #Variance of x

sd(x) #standard deviation of x

max(x) #max values in the list

min(x) #min value in the list

median(x) # Median or 50th percentile

weighted.mean(x, weight) #The sum of all values times a weight divided by the sum of the weights.

weightedMedian(x, weight) # library(matrixStats)

IQR(x) #inter quartile range

mad(x) #The mean of the absolute value of the deviations from the mean (l1-norm, Manhattan norm)



# Arima model code
# Book: Time series analysis and its application with R examples
# Author Shumway and Stooffer

set.seed(8675309) # Jenny, I got your number 
x = rnorm(150, mean=5) 
# generate iid N(5,1)s arima(x, order=c(1,0,1)) # estimation


par(mfrow = c(2,1))
plot(arima.sim(list(order=c(0,0,1), ma=.9), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==+.5)))
plot(arima.sim(list(order=c(0,0,1), ma=-.9), n=100), ylab="x",
     main=(expression(MA(1)~~~theta==-.5)))

ARMAtoMA(ar = .9, ma = .5, 10) # first 10 psi-weights
#[1] 1.40 1.26 1.13 1.02 0.92 0.83 0.74 0.67 0.60 0.54


#
#**************************************
# Code from Data camp
# Introduction to Time series analysis
# David S Matteson
#***************************************

# Basic commands to reveiw the data from a dataset

# Print the Nile dataset
print(Nile)

# List the number of observations in the Nile dataset
length(Nile)

# Display the first 10 elements of the Nile dataset
head(Nile, n=10)

# Display the last 12 elements of the Nile dataset
tail(Nile,n=12)



# Time series Analysis

# Plot the Nile data
plot(Nile)

# Plot the Nile data with xlab and ylab arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})")

# Plot the Nile data with xlab, ylab, main, and type arguments
plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})",main = "Annual River Nile Volume at Aswan, 1871-1970", type ="b")

# Plot the continuous_series using continuous time indexing
par(mfrow=c(2,1))
plot(continuous_time_index,continuous_series, type = "b")

# Make a discrete time index using 1:20 
discrete_time_index <-c(1:20)

# Now plot the continuous_series using discrete time indexing
plot(discrete_time_index,continuous_series, type = "b")

# View the start and end dates of AirPassengers
start(AirPassengers)
end(AirPassengers)

#The deltat() function returns the fixed time interval between observations and the frequency() 
#function returns the number of observations per unit time. Finally, the cycle() function returns 
#the position in the cycle of each observation.

# Use time(), deltat(), frequency(), and cycle() with AirPassengers 
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

# Convert data_vector to a ts object with start = 2004 and frequency = 4
time_series <- ts(data_vector,start=2004,frequency=4)

#check if the object is time series or not
is.ts(data_vector)

# Compute the mean of AirPassengers
mean(AirPassengers,na.rm=TRUE)

# Impute mean values to NA in AirPassengers
# to fill the missing values
AirPassengers[85:96] <- mean(AirPassengers, na.rm = TRUE)

# Add the complete AirPassengers data to your plot
rm(AirPassengers)
points(AirPassengers, type = "l", col = 2, lty = 3)

#To convert the timeseries into stationary we can use log and differentiation transformations

# Log rapid_growth
linear_growth <-log(rapid_growth)
dz <- diff(z)


#Simulate white noise model

# Simulate a WN model with list(order = c(0, 0, 0))
white_noise <- arima.sim(model = list(order = c(0, 0, 0)), n = 100)

# Simulate from the WN model with: mean = 100, sd = 10
white_noise_2 <- arima.sim(model = list(order = c(0, 0, 0)), n = 100, mean = 100, sd = 10)

# Fit the WN model to y using the arima command
arima(y,order=c(0,0,0))


# Random walk model

# Generate a RW model using arima.sim
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 100)

# Generate a RW model with a drift uing arima.sim
rw_drift <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, mean = 1)

# Now fit the WN model to the differenced data
model_wn <-arima(x=rw_diff,order=c(0,0,0))

# Use arima.sim() to generate WN data
white_noise <- arima.sim(model=list(order = c(0, 0, 0)),n=100)

# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)

# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model=list(order = c(0, 0, 0)),n=100,mean=0.4)

# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)

# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))


#colmeans used to calculate the mean for each column in the data set

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)

# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)

# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)

# Display a histogram of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")

#Calculating sample covariances and correlations

# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)

# Use cov() with logreturns
cov(logreturns)

# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)

# Use cor() with logreturns
cor(logreturns)

#Auto correlation function
# Use acf with x
acf(x, lag.max = 1, plot = FALSE)

# Generate ACF estimates for x up to lag-10
acf(x, lag.max = 10, plot = FALSE)


# Auto regressive model

#Use arima.sim() to simulate 100 observations of an AR model with slope equal to 0.5. To do so, set the
#model argument equal to list(ar = 0.5) and set the n argument equal to 100. Save this simulated data to x.

x <- arima.sim(model = list(ar = 0.5), n = 100)

# Comparison of random walk and auto  regressive models

# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar = 0.9), n = 200)
ts.plot(x)
acf(x)

# Simulate and plot AR model with slope 0.98
y <- arima.sim(model = list(ar = 0.98), n = 200)
ts.plot(y)
acf(y)

# Simulate and plot RW model
z <- arima.sim(model = list(order = c(0, 1, 0)), n = 200)
ts.plot(z)
acf(z)


#Forecast from an estimated AR model

# Fit an AR model to Nile
AR_fit <-arima(Nile, order  = c(1, 0, 0))
print(AR_fit)

# Use predict() to make a 1-step forecast
predict_AR <- predict(AR_fit,n.ahead=1)

# Obtain the 1-step forecast using $pred[1]
predict_AR$pred[1]

# Use predict to make 1-step through 10-step forecasts
predict(AR_fit, n.ahead = 10)

# Run to plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
AR_forecast <- predict(AR_fit, n.ahead = 10)$pred
AR_forecast_se <- predict(AR_fit, n.ahead = 10)$se
points(AR_forecast, type = "l", col = 2)

#Simulate moving average model

# Generate MA model with slope 0.5
x <- arima.sim(model = list(ma = 0.5), n = 100)


# Simple forecast from an estimated MA model
MA <- arima(Nile, order = c(0, 0, 1))

# Make a 1-step forecast based on MA
predict_MA <-predict(MA)

# Obtain the 1-step forecast using $pred[1]
predict_MA$pred[1]

# Make a 1-step through 10-step forecast based on MA
predict(MA,n.ahead=10)

# Plot the Nile series plus the forecast and 95% prediction intervals
ts.plot(Nile, xlim = c(1871, 1980))
MA_forecasts <- predict(MA, n.ahead = 10)$pred
MA_forecast_se <- predict(MA, n.ahead = 10)$se
points(MA_forecasts, type = "l", col = 2)
points(MA_forecasts - 2*MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecasts + 2*MA_forecast_se, type = "l", col = 2, lty = 2)


#To determine model fit, you can measure the Akaike information criterion (AIC) and Bayesian information
#criterion (BIC) for each model. While the math underlying the AIC and BIC is beyond the scope of this course,
#for your purposes the main idea is these these indicators penalize models with more estimated parameters, 
#to avoid overfitting, and smaller values are preferred. All factors being equal, a model that produces a 
#lower AIC or BIC than another model is considered a better fit.

# Find AIC of MA
AIC(MA)

# Find BIC of AR
BIC(AR)

#Code from datacamp xts
#********************************
#Manipulating Timeseries Data in R
# Jeff Ryan
#********************************


#More than a matrix
#It is best to think of xts objects as normal R matrices, but with special powers. These powers let you manipulate your data as a function of time, as your data is now self-aware of when it exists in time. Before we can start to exploit these powers, it will be helpful to see how xts objects relate to their base-R relatives.

#In this exercise, you will get a feel for xts and how it behaves like a matrix object. The xts object ex_matrix and matrix object core have been pre-loaded for you

# Load xts
library(xts)

# View the structure of ex_matrix
str(ex_matrix)

# Extract the 3rd observation of the 2nd column of ex_matrix
ex_matrix[3, 2]

# Extract the 3rd observation of the 2nd column of core 
core[3, 2]


# '''
# Your first xts object
# xts objects are simple. Think of them as a matrix of observations combined with an index of corresponding dates and times.
# 
# xts = matrix + times
# The main xts constructor takes a number of arguments, but the two most important are x for the data and order.by for the index. x must be a vector or matrix. order.by is a vector which must be the same length or number of rows as x, be a proper time or date object (very important!), and be in increasing order.
# 
# xts also allows you to bind arbitrary key-value attributes to your data. This lets you keep metadata about your object inside your object. To add these at creation, you simply pass additional name = value arguments to the xts() function.
# 
# Since we are focusing here on the mechanics, well use random numbers as our data so we can focus on creating the object rather than worry about its contents.
# 
# Create an object called data that contains five random numbers using rnorm().
# Create a Date class index from "2016-01-01" of length five called dates.
# Use the xts constructor to create an object called smith using data and dates as the index.
# Create an object called bday which contains a POSIXct date object containing the date "1899-05-08".
# Create an xts object called hayek using data, dates, and a new attribute called born, which should contain the birthday object you just created.
# '''

# Create the object data using 5 random numbers

data<-rnorm(5)
# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")

# Use xts() to create smith
smith <- xts(x = data, order.by = dates)

# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")

# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)

# '''
# Deconstructing xts
# Now that you can create xts objects, your next task is to examine an xts object from the inside.
# 
# At the core of both xts and zoo is a simple R matrix with a few additional attributes. The most important of these attributes is the index. The index holds all the information we need for xts to treat our data as a time series.
# 
# When working with time series, it will sometimes be necessary to separate your time series into its core data and index attributes for additional analysis and manipulation. The core data is the matrix portion of xts. You can separate this from the xts object using coredata(). The index portion of the xts object is available using the index() function. Note that both of these functions are methods from the zoo class, which xts extends.
# 
# In this exercise you will use these built-in functions to extract both the internal matrix data and internal index from your sample xts object. You will use the hayek time series you created in the last exercise to practice these new functions.
# '''

 # Extract the core data of hayek
 hayek_core<-coredata(hayek)
 
 # View the class of hayek_core
 class(hayek_core)
# [1] "matrix"
 
 # Extract the index of hayek
 hayek_index<-index(hayek)
 
 # View the class of hayek_index
 class(hayek_index)
# [1] "Date"


 # Convert austres to an xts object called au
 au <- as.xts(austres)
 
 # Then convert your xts object (au) into a matrix am
 am <- as.matrix(au)
 
 # Inspect the head of am
 head(am)
# au
# 1971 Q2 13067.3
# 1971 Q3 13130.5
# 1971 Q4 13198.4
# 1972 Q1 13254.2
# 1972 Q2 13303.7
# 1972 Q3 13353.9
 
 # Convert the original austres into a matrix am2
 am2<-as.matrix(austres)
 
 # Inspect the head of am2
 head(am2)

 

# Create dat by reading tmp_file
dat<-read.csv(tmp_file)

# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")

# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)


# Convert sunspots to xts using as.xts().
sunspots_xts<-as.xts(sunspots)

# Get the temporary file name
tmp <- tempfile()

# Write the xts object using zoo to tmp 
write.zoo(sunspots_xts, sep = ",", file = tmp)

# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class
sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)

# Convert sun into xts. Save this as sun_xts
sun_xts<-as.xts(sun)

# # '''
# Querying for dates
# One of the most powerful aspects of working with time series in xts is the ability to quickly and efficiently specify dates and time ranges for subsetting.
# 
# Date ranges can be extracted from xts objects by simply specifying the period(s) you want using special character strings in your subset.
# 
# A["20090825"]       ## Aug 25, 2009
# A["201203/201212"]  ## Mar to Dec 2012
# A["/201601"]        ## Up to and including January 2016
# For this exercise you will create a simple but very common query. Extract a range of dates using the ISO-8601 feature of xts. After successfully extracting a full year, you will then create a subset of your new object with specific start and end dates using this same notation.
# 
# Lets find some time!
#   
#   
#   Matrix arithmetic - add, subtract, multiply, and divide in time!
#   xts objects respect time. By design when you perform any binary operation using two xts objects, these objects are first aligned using the intersection of the indexes. This may be surprising when first encountered.
# 
# The reason for this is that you want to preserve the point-in-time aspect of your data, assuring that you dont introduce accidental look ahead (or look behind!) bias into your calculations.
# 
# What this means in practice is that you will sometimes be tasked with handling this behavior if you want to preserve the dimensions of your data.
# 
# Your options include:
# 
# Use coredata() or as.numeric() (drop one to a matrix or vector).
# Manually shift index values - i.e. use lag().
# Reindex your data (before or after the calculation).
# In this exercise, youll look at the normal behavior, as well as an example using the first option. For now you will use two small objects a and b. Examine these objects in the console before you start.
# '''

# a
# 2015-01-24 1
# 2015-01-25 1
# 2015-01-26 1

b
# 2015-01-24 2
 # Add a and b
a+b
a
#2015-01-24 3
 
   # Add a with the numeric value of b
   a+as.numeric(b)
# a
# 2015-01-24 3
# 2015-01-25 3
# 2015-01-26 3
 
  
   x_2016 <- x["2016"]
 
   # Select January 1, 2016 to March 22, 2016
   jan_march <- x["2016/2016-03-22"]
 
   # Verify that jan_march contains 82 rows
   82 == length(jan_march)

# Combining xts by column with merge
# xts makes it easy to join data by column and row using a few different functions. All results will be correctly ordered in time, regardless of original frequencies or date class. One of the most important functions to accomplish this is merge(). It takes one or more series and joins them by column. Its also possible to combine a series with a vector of dates. This is especially useful for normalizing observations to a fixed calendar.
# 
# merge() takes three key arguments which we will emphasize here. First is the ..., which lets you pass in an arbitrary number of objects to combine. The second argument is join, which specifies how to join the series - accepting arguments such as inner or left. This is similar to a relational database join, only here, the index is what we join on. The final argument for this exercise is fill. This keyword specifies what to do with the new values in a series if there is missingness introduced as a result of the merge.
# '''
# Basic argument use
merge(a, b, join = "right", fill = 9999)
#For this exercise, you will explore some of the different join types to get a feel for using merge(). The objects a and b have been pre-loaded into your workspace.


# Perform an inner join of a and b
merge(a, b, join = "inner")
#a          b
# 2016-06-05 -1.2070657  0.4291247
# 2016-06-08  0.2774292 -0.5747400
# 2016-06-09  1.0844412 -0.5466319
 
 # Perform a left-join of a and b, fill missing values with 0
 merge(a, b, join = "left", fill = 0)
#a          b
# 2016-06-05 -1.2070657  0.4291247
# 2016-06-08  0.2774292 -0.5747400
# 2016-06-09  1.0844412 -0.5466319
# 2016-06-13 -2.3456977  0.0000000


# Row bind temps_june30 to temps, assign this to temps2
temps2 <- rbind(temps_june30, temps)

# Row bind temps_july17 and temps_july18 to temps2, call this temps3
temps3 <- rbind(temps_july17, temps_july18, temps2)


# '''
# Apply a function by time period(s)
# At this point you know how to locate the end of periods using endpoints(). You may be wondering what it is you do with these values.
# 
# In the most simple case you can subset your object to get the last values. In certain cases this may be useful. For example, to identify the last known value of a sensor during the hour or get the value of the USD/JPY exchange rate at the start of the day. For most series, you will want to apply a function to the values between endpoints. In essence, use the base function apply(), but used on a window of time.
# 
# To do this easily, xts provides the period.apply() command, which takes a time series, an index of endpoints, and a function.
# 
# period.apply(x, INDEX, FUN, ...)
# In this exercise youll practice using period.apply() by taking the weekly mean of your temps data. Youll also look at one of the shortcut functions that does the same thing with slightly different syntax.
# '''
#  # Locate the weeks
   endpoints(temps, on = "weeks")

 
   # Locate every two weeks
   endpoints(temps, on = "weeks", k = 2)



 # Split temps by week
   temps_weekly <- split(temps, f = "weeks")
 
   # Create a list of weekly means, temps_avg, and print this list
   temps_avg <- lapply(X = temps_weekly, FUN = mean)
 temps_avg



 # Use the proper combination of split, lapply and rbind
   temps_1 <- do.call(rbind, lapply(split(temps, "weeks"), function(w) last(w, n = "1 day")))
 
   # Create last_day_of_weeks using endpoints()
   last_day_of_weeks <- endpoints(temps,on="weeks")
 
   # Subset temps using last_day_of_weeks
   temps_2 <- temps[last_day_of_weeks]


 # Convert usd_eur to weekly and assign to usd_eur_weekly
 usd_eur_weekly <- to.period(usd_eur, period = "weeks")
 
 # Convert usd_eur to monthly and assign to usd_eur_monthly
 usd_eur_monthly <- to.period(usd_eur, period = "months")
 
 # Convert usd_eur to yearly univariate and assign to usd_eur_yearly
 usd_eur_yearly <- to.period(usd_eur, period = "years", OHLC = FALSE)

# '''Calculate basic rolling value of series by month
# One common aggregation you may want to apply involves doing a calculation within the context of a period, but returning the interim results for each observation of the period.
# 
# For example, you may want to calculate a running month-to-date cumulative sum of a series. This would be relevant when looking at monthly performance of a mutual fund you are interested in investing in.
# 
# For this exercise, youll calculate the cumulative annual return using the edhec fund data from the last exercise. To do this, youll follow the split()-lapply()-rbind() pattern demonstrated below:
# '''

x_split <- split(x, f = "months")
x_list <- lapply(x_split, cummax)
x_list_rbind <- do.call(rbind, x_list)

#Note the last call uses R's somewhat strange do.call(rbind, ...) syntax, which allows you to pass a list to rbind instead of passing each object one at a time. This is a handy shortcut for your R toolkit.

 # Split edhec into years
   edhec_years <- split(edhec , f = "years")
 
   # Use lapply to calculate the cumsum for each year in edhec_years
   edhec_ytd <- lapply(edhec_years, FUN = cumsum)
 
   # Use do.call to rbind the results
   edhec_xts <- do.call(rbind, edhec_ytd)




 # View the first three indexes of temps
   index(temps)[1:3]

 
   # Get the index class of temps
   indexClass(temps)

 
   # Get the timezone of temps
   indexTZ(temps)
TZ 
"America/New_York"
 
   # Change the format of the time display
   indexFormat(temps) <- "%b-%d-%Y"
 

 periodicity(temps)
#Daily periodicity from 2016-07-01 to 2016-07-16
 
   # Calculate the periodicity of edhec
   periodicity(edhec)
#Monthly periodicity from 1997-01-31 to 2009-08-31
 
   # Convert edhec to yearly
   edhec_yearly <- to.yearly(edhec)
 
   # Calculate the periodicity of edhec_yearly
   periodicity(edhec_yearly)
#Yearly periodicity from 1997-12-31 to 2009-08-31

 # Count the months
   nmonths(edhec)

 
   # Count the quarters
   nquarters(edhec)

 
   # Count the years
   nyears(edhec)

 # Make z have unique timestamps
   z_unique <- make.index.unique(z, eps = 1e-4)

   # Remove duplicate times in z
   z_dup <- make.index.unique(z, drop = TRUE)
 
   # Round observations in z to the next hour
   z_round <- align.time(z, n = 3600)

