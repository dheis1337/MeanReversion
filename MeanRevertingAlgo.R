library(Quandl)
library(TTR)
library(ggplot2)
library(dplyr)
library(data.table)


data1 <- Quandl("ECB/EURUSD", order = "asc")

data2 <- BBands(data1$Value, n = 30, sd = 3)
data2 <- data.frame(cbind(data1, data2))

EURUSD <- as.data.table(data2)
EURUSD <- EURUSD[-c(1:29)]
EURUSD[, low.eps := mavg -(mavg * .005)]
EURUSD[, up.eps := mavg +(mavg * .005)]

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

# Run through the dates that the Value is below the dn band in EURUSD (saved in below) and 
# add a 30 day timeframe after the date. Then reference the between.lowers data.table and 
# and create check. Then if the l
count <- 0
for(i in 1:length(below.date)){
        date[i] <-  + 1:30
        check <- between.low.and.eps[Date %in% date, ]
        if(length(check[Value > lower.e2ps]) > 1){
          count <- count + 1
          
        }
      }


clean.date <- function(x){
        dates <<- difftime()
}

below.test <- function(x){
 if(x[Value < dn]){
  date <-  + 1:30
  check <- between.low.and.eps[Date %in% date, .(Value, lower.eps)]
  if(sum(check[Value > lower.e2ps]) > 1){
    count <- count + 1
    }
  }
}

data2 <- data2[-c(1:4000), ]

ggplot(test, aes(x = Date)) +
    geom_line(aes(y = Value), col = "blue") +
    geom_line(aes(y = mavg)) +
    geom_line(aes(y = dn), col = "red") +    
    geom_line(aes(y = up),col = "red" ) +
    geom_line(aes(y = low.eps))
    

data2

abv <- dummy[dummy$Value > dummy$up, ]
blow <- dummy[dummy$Value < dummy$dn, ]




eps <- data2$mavg * 0.005
up.eps <- data2$mavg + eps

up <- data2$up


dummy <- data.frame(data2)
dummy <- mutate(dummy, up.eps = (dummy$mavg * .005) + dummy$mavg)
dummy <- mutate(dummy, low.eps = dummy$mavg - (dummy$mavg * .005))



ggplot(dummy, aes(x = data2.Date)) +
      geom_line(aes(y = data2.mavg), col = "blue") +
      geom_line(aes(y = up.eps)) +
      geom_line(aes(y = low.eps))

extend.date <- function(x){
          dates <<- x + 1:30
          
below[, Date2 := as.Date(c(below$Date[-1], NA))] # Line up Date column with the succeeding date


below$Date2 - below$Date # Take difference between Date column and succeeding date column
below[below$Date2 - below$Date > 1] # Find where difference in Dates is greater than 1

test <- below[below$Date2 - below$Date > 1]


test2 <- EURUSD[Date %in% test]

success <- 0
if(nrow(test2[Value > low.eps]) > 0){
  success <- success + 1
}
belowdates <- below[, Date]

 CreateRange <- function(x, timeframe){
   # Take a vector of dates preprocessed to be the last dates where the 
   # Value < dn band and then adds the specified 'timeframe' to them. Could
   # be 30 periods, could be 15, etc
   x + 1:timeframe 
 }

# Created variable to test how to lapply above function properly
daterange <- lapply(test$Date, CreateRange, 30)

DownMatch <- function(x, matchdata){
  # Takes the result from the CreateRange function and matches the dates in this
  # result with the dates in the overall dataset. 
  matchdata[Date %in% x]
}

# Created variable to test how to lapply above function properly
testrange <- lapply(daterange, DownMatch, EURUSD)

success <- 0

TestCounter <- function(x){
  # Takes the result of prior function and then tests to see if there is anywhere
  # where the Value has officially crossed back over the low.eps mavg error band, thus
  # indicating that it has successfully bounced back from being below the desired standard
  # deviation level, i.e our value has officially reverted!
    if(nrow(x[Value > low.eps]) > 0){
    success <- success + 1
  }
}

# Not sure I still need this
RemoveNull <- function(x){
    !is.null(x)
}
# Created to test prior function 
success2 <- sapply(success, RemoveNull) # This should actually be called success

sum(success2) / length(test$Date)


# Finally got something that works. Use above functions in one function call to find answer. 
DownCounter <- function(data) {
  data[, Date2 := as.Date(c(data$Date[-1], NA))]
  dates <<- data[data$Date2 - data$Date > 1]
  daterange1 <<- lapply(dates$Date, CreateRange, 30)
  testrange1 <<- lapply(daterange1, DownMatch, EURUSD)
  positive <<- sapply(testrange1, TestCounter)
  positive1 <<- unlist(positive)
  sum(unlist(positive1)) / length(dates$Date)
}

