library(Quandl)
library(TTR)
library(ggplot2)
library(dplyr)
library(data.table)

# Get data from Quandl
EURUSD <- Quandl("ECB/EURUSD", order = "asc")

# Add standard deviation bands
bands <- BBands(EURUSD$Value, n = 30, sd = 3)

# Add standard deviations to 'EURUSD'
EURUSD <- cbind(EURUSD, bands)

# Get rid 'bands' variable; won't be used anymore 
rm(bands)

# Make 'EURUSU' data.table
EURUSD <- as.data.table(EURUSD)


# Remove NA values created from calculation in above code and remove 'pctB' column
EURUSD[, pctB := NULL]
EURUSD <- EURUSD[-c(1:29)]

# Add error bands to the moving average to ease the mean reverting requirement
EURUSD[, low.eps := mavg -(mavg * .005)]
EURUSD[, up.eps := mavg +(mavg * .005)]

# Make data.table for below deviation
below <- EURUSD[Value < dn]

# Make data.table for above deviation
above <- EURUSD[Value > up]

# Below are a list of functions to used in the final MeanRevert function

CleanData <- function(x){
  # Takes either the 'below' or 'above' data.table and takes only the last day
  # where the 'Value' is below/above the respective band, i.e. it removes any 
  # consecutive days below/above the respective band and just takes the last day
  # before crossing back inside the band
  x[, Date2 := as.Date(c(x$Date[-1], NA))]
  dates <- x[x$Date2 - x$Date > 1]
  dates <- rbind(dates, x[.N])
}

 CreateRange <- function(x, timeframe){
   # Take a vector of dates preprocessed to be the last dates where the 
   # Value < dn band and then adds the specified 'timeframe' to them. Could
   # be 30 periods, could be 15, etc
   x + 1:timeframe 
 }


DateMatch <- function(x, matchdata){
  # Takes the result from the CreateRange function and matches the dates in this
  # result with the dates in the overall dataset. 
  matchdata[Date %in% x]
}


TestCounter <- function(x){
  # Takes the result of prior function and then tests to see if there is anywhere
  # where the Value has officially crossed back over the low.eps mavg error band, thus
  # indicating that it has successfully bounced back from being below the desired standard
  # deviation level, i.e our value has officially reverted!
  success <- 0
  if(nrow(x[Value > low.eps]) > 0){
  success <- success + 1
  }
}

# Created to test prior function 
sapply(success, RemoveNull) # This should actually be called success
sum(success2) / length(test$Date)


MeanRevert <- function(data){
  # The final function in the sequence. Takes the preprocessed data.table
  # that is corresponds to the times in our dataset where the 'Value' 
  # goes below the 'dn' st.dev band and reverts back to the 'mavg'. Final
  # result is the prob. that there is a succesful reversion; defined as the 
  # number of times reverted over the number of nonconsecutive days 'Value'
  # is below the 'dn' band. 
  dates <- CleanData(data)
  daterange1 <- lapply(dates$Date, CreateRange, 30)
  testrange1 <- lapply(daterange1, DateMatch, EURUSD)
  positive <- sapply(testrange1, TestCounter)
  positive1 <- unlist(positive)
  sum(unlist(positive1)) / length(dates$Date)
}




