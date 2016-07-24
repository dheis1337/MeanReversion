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


# Make below table
lower <- EURUSD[, .(Date, Value, dn, mavg)]
lower[, low.eps := mavg -(mavg * .005)]

# Make above data.table
upper <- EURUSD[, .(Date, Value, up, mavg)]
upper[, up.eps := mavg +(mavg * .005)]

# Below
below <- EURUSD[Value < dn]

# Above 
above <- EURUSD[Value > up]

# Between lower epsilon band and lower bollinger band
between.low.and.eps <- EURUSD[Value > dn & Value < low.eps]

# Between upper epsilon band and upper bollinger
between.up.and.eps <- EURUSD[Value < up & Value > up.eps]

# Between epsilon bands
between.eps <- EURUSD[Value >= low.eps & Value <= up.eps]


data2 <- data2[-c(1:4000), ]

abv <- dummy[dummy$Value > dummy$up, ]
blow <- dummy[dummy$Value < dummy$dn, ]


eps <- data2$mavg * 0.005
up.eps <- data2$mavg + eps

up <- data2$up


dummy <- data.frame(data2)
dummy <- mutate(dummy, up.eps = (dummy$mavg * .005) + dummy$mavg)
dummy <- mutate(dummy, low.eps = dummy$mavg - (dummy$mavg * .005))




below[, Date2 := as.Date(c(below$Date[-1], NA))] # Line up Date column with the succeeding date


below$Date2 - below$Date # Take difference between Date column and succeeding date column
below[below$Date2 - below$Date > 1] # Find where difference in Dates is greater than 1

below[below$Date2 - below$Date > 1]




success <- 0
if(nrow(test2[Value > low.eps]) > 0){
  success <- success + 1
}


 CreateRange <- function(x, timeframe){
   # Take a vector of dates preprocessed to be the last dates where the 
   # Value < dn band and then adds the specified 'timeframe' to them. Could
   # be 30 periods, could be 15, etc
   x + 1:timeframe 
 }


DownMatch <- function(x, matchdata){
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

# Not sure I still need this
RemoveNull <- function(x){
    !is.null(x)
}
# Created to test prior function 
sapply(success, RemoveNull) # This should actually be called success
sum(success2) / length(test$Date)


# Finally got something that works. Use above functions in one function call to find answer. 
DownCounter <- function(data) {
  success <- 0
  data[, Date2 := as.Date(c(data$Date[-1], NA))]
  dates <- data[data$Date2 - data$Date > 1]
  daterange1 <- lapply(dates$Date, CreateRange, 30)
  testrange1 <- lapply(daterange1, DownMatch, EURUSD)
  positive <- sapply(testrange1, TestCounter)
  positive1 <- unlist(positive)
  sum(unlist(positive1)) / length(dates$Date)
}

