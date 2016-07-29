library(TTR)
library(ggplot2)
library(data.table)
library(lubridate)

# Path
path <- c("C:/MyStuff/DataScience/Projects/MeanReversion")
setwd(path)

# Get USDJPY data ==============================================================
USDJPY <- fread("USDJPY.csv", header = FALSE,
                col.names = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"))

# Desired currencies
currencies <- c("USDJPY.csv", "EURUSD.csv")

# Import data function
currency.list <- list()
ImportData <- function(currencies){
                for (i in 1:length(currencies)){
                 currency <- fread(currencies[i], header = FALSE, 
                        col.names = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"))
                  names <- c(currencies[i]) 
                  tmp <- data.table(currency)
                  currency.list[[names]] <<- tmp
                }
} 


# Formatting data ==============================================================
FormatData <- function(data, time, sd, periods){ 
  # Function takes as inputs the raw data from the Meta Trader 4 platform 
  # and converts it into the necessary two data.tables - 'above' and 'below' - 
  # to be used in the MeanRevert function. The inputs are:
  # data: raw data 
  # time: number of days you want to look back from the current date
  # sd: number of standard deviations to set the Bollinger bands to
  # periods: number of periods you want to look forward to test if the price 
  # has rever
  data[, Date := gsub("\\.", "-", data$Date), ] # Change date format
  dates <- Sys.Date() - days(1:time) # Used to subset data
  dates <- as.character(dates) # Convert for subsetting
  names(data) <- c("Date", "Date.Time","Open", "High", "Low", "Close", "Volume")
  data <- data[Date %in% dates] # Gets last 1:time number of dates
  data[, Date.Time := paste(data$Date, data$Date.Time, sep = " ")]
  data[, c("Date", "Open", "High", "Low", "Volume") := NULL]
  data[, Date.Time := as.POSIXct(data$Date.Time)]
  data <- data[order(Date.Time)]
  bands <- BBands(data$Close, n = periods, sd = sd)
  data <- cbind(data, bands)
  data[, pctB := NULL]
  data <- data[-c(1:119)]
  data[, low.eps := mavg - .0015]
  data[, up.eps := mavg + .0015]
  matchdata <<- data
  below <<- data[Close < dn]
  below[, c("up", "up.eps") := NULL]
  above <<- data[Close > up]
  above[, c("dn", "low.eps") := NULL]
  
}


# Below are a list of functions to used in the final MeanRevert function =======
CleanData <- function(x, width){
  # Takes either the 'below' or 'above' data.table and takes only the last day
  # where the 'Value' is below/above the respective band, i.e. it removes any 
  # consecutive days below/above the respective band and just takes the last day
  # before crossing back inside the band
  x[, Date.Time2 := as.POSIXct(c(x$Date.Time[-1], NA))]
  dates <- x[x$Date.Time2 - x$Date.Time > width, ]
  dates <- rbind(dates, x[.N])
}

CreateRange <- function(x, timeframe){
   # Take a vector of dates preprocessed to be the last dates where the 
   # Value < dn band and then adds the specified 'timeframe' to them. Could
   # be 30 periods, could be 15, etc
   x + minutes(1:timeframe) 
 }


DateMatch <- function(x){
  # Takes the result from the CreateRange function and matches the dates in this
  # result with the dates in the overall dataset. 
  matchdata[Date.Time %in% x]
}



TestCounterBelow <- function(x){
  # Takes the result of prior function and then tests to see if there is anywhere
  # where the Value has officially crossed back over the low.eps mavg error band, thus
  # indicating that it has successfully bounced back from being below the desired standard
  # deviation level, i.e our value has officially reverted!
  success <- 0
  if(nrow(x[Close > low.eps]) > 0){
    success <- success + 1
  }
}

TestCounterAbove <- function(x){
  # Takes the result of prior function and then tests to see if there is anywhere
  # where the Value has officially crossed back over the low.eps mavg error band, thus
  # indicating that it has successfully bounced back from being below the desired standard
  # deviation level, i.e our value has officially reverted!
  success <- 0
  if(nrow(x[Close < up.eps]) > 0){
    success <- success + 1
  }
}



MeanRevert <- function(data, width, timerange, type){
  # The final function in the sequence. Takes the preprocessed data.table
  # that is corresponds to the times in our dataset where the 'Value' 
  # goes below the 'dn' st.dev band and reverts back to the 'mavg'. Final
  # result is the prob. that there is a succesful reversion; defined as the 
  # number of times reverted over the number of nonconsecutive days 'Value'
  # is below the 'dn' band. 
  times <- CleanData(data, width)
  range <- lapply(times$Date.Time, CreateRange, timerange)
  testrange <- lapply(range, DateMatch)
  if (type == "below"){
    positive <- sapply(testrange, TestCounterBelow)
    x <- sum(unlist(positive)) / length(times$Date.Time)
  } else if (type == "above") {
    positive <- sapply(testrange, TestCounterAbove)
    x <- sum(unlist(positive)) / length(times$Date.Time)
  }
  
  return(x)
}



# Look into changing the overall function to removing all instances where there 
# is 
below <- EURUSD[Close < dn]
below[, Date.Time2 := as.POSIXct(c(below$Date.Time[-1], NA))]
dates <- below[below$Date.Time2 - below$Date.Time > 10]
dates <- rbind(dates, below[.N])
range <- lapply(dates$Date.Time, CreateRange, 10)
test <- lapply(range, DateMatch)
positive <- sapply(test, TestCounter, type = "below")
sum(unlist(positive)) / length(dates$Date.Time)

above[,Date.Time2 := as.POSIXct(c(above$Date.Time[-1], NA))]
dates1 <- above[above$Date.Time2 - above$Date.Time > 30, ]
dates1 <- rbind(dates1, above[.N])
range1 <- lapply(dates1$Date.Time, CreateRange, 30)
test1 <- lapply(range1, DateMatch)
positive <- sapply(test1, TestCounterAbove)
sum(unlist(positive)) / length(dates1$Date.Time)

dt <- data.table()
for (i in 1:length(testlist)){
  result <- mean(testlist[[1]])

}

testlist <- list(Currency = c("USDJPY", "AUDCAD"), Above = c(.38, .734), Below = c(.34, .88))
as.data.table(testlist)

below
