library(TTR)
library(ggplot2)
library(data.table)

# Get data 
EURUSD <- fread("C:/MyStuff/DataScience/Projects/MeanReversion/EURUSD1.csv", header = FALSE,
      col.names = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"), data.table = FALSE)

# Current format of 'Time' variable is character. Let's change that to POSIXct 
# First let's make the '.' in the 'Date' variable '-'
EURUSD$Date <- gsub("\\.", "-", EURUSD$Date)

# Now let's join the 'Date' and 'Time' variables
EURUSD$Time <- paste(EURUSD$Date, EURUSD$Time, sep = " ")

# Now let's convert the 'Time' variable to POSIXct
EURUSD$Time <- as.POSIXct(EURUSD$Time)

# Now let's remove the 'Date' Column sense we have our Datetime format in 'Time'
EURUSD <- EURUSD[, -1]

# Let's change the column names
names(EURUSD) <- c("Date.Time", "Open", "High", "Low", "Close", "Volume")

# Finally, let's make EURUSD a data.table
EURUSD <- data.table(EURUSD)

# Add standard deviation bands
bands <- BBands(EURUSD$Close, n = 30, sd = 3)

# Add standard deviations to 'EURUSD'
EURUSD <- cbind(EURUSD, bands)

# Get rid 'bands' variable; won't be used anymore 
rm(bands)


# Remove NA values created from calculation in above code and remove 'pctB' column
EURUSD[, c("pctB", "Open", "High", "Low", "Volume") := NULL]
EURUSD <- EURUSD[-c(1:29)]

# Add error bands to the moving average to ease the mean reverting requirement
EURUSD[, low.eps := mavg -(mavg * .005)]
EURUSD[, up.eps := mavg +(mavg * .005)]

# Make data.table for below deviation
below <- EURUSD[Close < dn]

# Make data.table for above deviation
above <- EURUSD[Close > up]

# Below are a list of functions to used in the final MeanRevert function

CleanData <- function(x){
  # Takes either the 'below' or 'above' data.table and takes only the last day
  # where the 'Value' is below/above the respective band, i.e. it removes any 
  # consecutive days below/above the respective band and just takes the last day
  # before crossing back inside the band
  x[, Date.Time2 := as.POSIXct(c(below$Date.Time[-1], NA))]
  dates <- x[x$Date.Time2 - x$Date.Time > 1,]
  dates <- rbind(dates, x[.N])
  return(dates)
}

 CreateRange <- function(x, timeframe){
   # Take a vector of dates preprocessed to be the last dates where the 
   # Value < dn band and then adds the specified 'timeframe' to them. Could
   # be 30 periods, could be 15, etc
   x + minutes(1:timeframe) 
 }


DateMatch <- function(x, matchdata){
  # Takes the result from the CreateRange function and matches the dates in this
  # result with the dates in the overall dataset. 
  matchdata[Date.Time %in% x]
}


TestCounter <- function(x){
  # Takes the result of prior function and then tests to see if there is anywhere
  # where the Value has officially crossed back over the low.eps mavg error band, thus
  # indicating that it has successfully bounced back from being below the desired standard
  # deviation level, i.e our value has officially reverted!
  success <- 0
  if(nrow(x[Close > low.eps]) > 0){
  success <- success + 1
  }
}


MeanRevert <- function(data){
  # The final function in the sequence. Takes the preprocessed data.table
  # that is corresponds to the times in our dataset where the 'Value' 
  # goes below the 'dn' st.dev band and reverts back to the 'mavg'. Final
  # result is the prob. that there is a succesful reversion; defined as the 
  # number of times reverted over the number of nonconsecutive days 'Value'
  # is below the 'dn' band. 
  dates <- CleanData(data)
  daterange <- lapply(dates$Date.Time, CreateRange, 30)
  testrange <- lapply(daterange, DateMatch, EURUSD)
  positive <- sapply(testrange, TestCounter)
  sum(unlist(positive)) / length(dates$Date.Time)
}



# Look into changing the overall function to removing all instances where there 
# is 
below <- EURUSD[Close < dn]
below[, Date.Time2 := as.POSIXct(c(below$Date.Time[-1], NA))]
dates <- below[below$Date.Time2 - below$Date.Time > 1]
dates <- rbind(dates, below[.N])
range <- lapply(dates$Date.Time, CreateRange, 30)
test <- lapply(range, DateMatch, EURUSD)
positive <- sapply(test, TestCounter)
sum(unlist(positive)) / length(dates$Date.Time)
