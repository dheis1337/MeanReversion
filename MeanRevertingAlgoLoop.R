library(TTR)
library(data.table)
library(lubridate)
library(dplyr)

setwd("C:/MyStuff/DataScience/Projects/MeanReversion")

currencies <- c("eurusd1.csv", "GBPUSD.csv")


ImportData <- function(x) {
    # Loop over the vector with the desired currencies and create a list 
    # of data.tables for each one and reorders the data.tables in sequential order
    # by dates
    for (i in 1:length(x)) {
      currency <- fread(x[i], header = FALSE, select = c(1, 5, 9),
                        col.names = c("Date", "Close.Ask", "Close.Bid"),
                        skip = 1)
      tmp <- data.table(currency)
      currency.list[[i]] <<- tmp # fill in currency.list element i 
      currency.list[[i]] <<- currency.list[[i]][order(Date)] # order dates
    }
}

############### Algorithm workflow up until above function ################## 
rm(currency.list)
currency.list <- list()
ImportData(currencies)
#############################################################################

######################### Function to clean dates ############################

DateClean <- function(x) {
    # Takes currency.list created from ImportData function and converts Date 
    # variable to a POSIXct class
    for (i in 1:length(x)) {
      x[[i]][, Date := gsub("/", "-", x[[i]]$Date)]
      x[[i]][, Date := parse_date_time(x[[i]]$Date, orders = c("%m-%d-%Y %H:%M:%S"),
                                       tz = "America/New_York")]
      
  }
}

###################### Algorithm workflow up until above function ##############
rm(currency.list)
currency.list <- list()
ImportData(currencies)
DateClean(currency.list)
################################################################################

################## Function to deal with the prices ############################

PriceClean <- function(x, sd, periods) {
  # First, create a list for the sd, mavg data. Then, average the Close.Ask and 
  # Close.Bid so there is one price to calculate sd, mavg with. Remove Close.Ask
  # and Close.Bid. Calculate sd, mavg. Ammend each currency's data.table to contain
  # calculated values
  bband.data <- list() # create sd, mavg list
  for (i in 1:length(x)) {
    x[[i]][, Price := ((Close.Ask + Close.Bid) / 2)] # average Close.Ask and Close.Bid
    x[[i]][, c("Close.Ask", "Close.Bid") := NULL] # remove these columns
    bband.data[[i]] <- BBands(x[[i]]$Price, n = periods, sd = sd) # calculations
    x[[i]][, c("dn", "mavg", "up", "pctB") := list(bband.data[[i]][, 1], # Add values to
                                                   bband.data[[i]][, 2], # data.tables
                                                   bband.data[[i]][, 3],
                                                   bband.data[[i]][, 4])]
    x[[i]][, pctB := NULL] # remove pctB
    x[[i]][-c(1:periods) ]
  }
  
}



########## Algorithm workflow on one data.table up until PriceClean ###########
rm(currency.list)
currency.list <- list()
ImportData(currencies)
DateClean(currency.list)
PriceClean(currency.list, sd = 2, periods = 30)
###############################################################################

rm(eurusd)
eurusd <- fread("eurusd1.csv", header = FALSE, select = c(1, 5, 9),
                col.names = c("Date", "Close.Ask", "Close.Bid"),
                skip = 1)
eurusd[, gsub("/", "-", eurusd$Date)]
eurusd[, Date := parse_date_time(eurusd$Date, orders = c("%m-%d-%Y %H:%M:%S"), tz = "GMT")]
eurusd <- eurusd[order(Date)]
eurusd[, Price := ((Close.Ask + Close.Bid) / 2)]
bband.data <- BBands(eurusd$Price, n = 30, sd = 2)
eurusd <- cbind(eurusd, bband.data)
eurusd[, c("pctB", "Close.Ask", "Close.Bid") := NULL]
###############################################################################


dates <- list()
FormatDates <- function (x, type, width) {
    if (type == "below") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price < dn]
        dates[[i]] <- dates[[i]][, Date2 := as.POSIXct(c(dates[[i]]$Date[-1], NA),
                                                tz = "America/New_York")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <<- dates[[i]][(dates[[i]]$Date2 - dates[[i]]$Date) > width]
        dates[[i]] <- rbind(dates[[i]], lastrow)
  }
}   else if (type == "above") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price > up]
        dates[[i]] <- dates[[i]][, Date2 := as.POSIXct(c(dates[[i]]$Date[-1], NA),
                                                       tz = "America/New_York")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <- dates[[i]][(dates[[i]]$Date2 - dates[[i]]$Date) > width]
        dates[[i]] <<- rbind(dates[[i]], lastrow)
    }
  }
}


rm(eurusd)
eurusd <- fread("eurusd1.csv", header = FALSE, select = c(1, 5, 9),
                col.names = c("Date", "Close.Ask", "Close.Bid"),
                skip = 1)
eurusd[, gsub("/", "-", eurusd$Date)]
eurusd[, Date := parse_date_time(eurusd$Date, orders = c("%m-%d-%Y %H:%M:%S"), tz = "America/Denver")]
eurusd <- eurusd[order(Date)]
eurusd[, Price := ((Close.Ask + Close.Bid) / 2)]
bband.data <- BBands(eurusd$Price, n = 30, sd = 2)
eurusd <- cbind(eurusd, bband.data)
eurusd[, c("pctB", "Close.Ask", "Close.Bid") := NULL]
test.dates <- eurusd[, Date[-1]]
eurusd[, Date2 := as.POSIXct(c(eurusd$Date[-1], NA), tz = "America/Denver")]
eurusd[1][, Date2]
eurusd[1][, Date]

eurusd[Date2 - Date > 3,]

down.test <- eurusd[Price < dn]
down.test[, Date1 := c(down.test$Date[-1], NA)]
?parse_date_time

  