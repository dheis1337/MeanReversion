library(TTR)
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

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


DateClean <- function(x) {
    # Takes currency.list created from ImportData function and converts Date 
    # variable to a POSIXct class
    for (i in 1:length(x)) {
      x[[i]][, Date := gsub("/", "-", x[[i]]$Date)]
      x[[i]][, Date := parse_date_time(x[[i]]$Date, orders = c("%m-%d-%Y %H:%M:%S"),
                                       tz = "America/New_York")]
    }
}


################## Function to deal with the prices ############################
ref.list <- vector("list", length(currency.list))
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
    ref.list[[i]] <<- x[[i]]
  }
  
}


FormatDates <- function (x, type, width) {
    if (type == "below") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price < dn]
        dates[[i]] <- dates[[i]][, Date2 := as.POSIXct(c(dates[[i]]$Date[-1], NA),
                                              tz = "America/Denver")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <- dates[[i]][(dates[[i]]$Date2 - dates[[i]]$Date) > width]
        dates[[i]] <- rbind(dates[[i]], lastrow)
        currency.list[[i]] <<- dates[[i]]
  }
}   else if (type == "above") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price > up]
        dates[[i]] <- dates[[i]][, Date2 := as.POSIXct(c(dates[[i]]$Date[-1], NA),
                                                       tz = "America/Denver")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <- dates[[i]][(dates[[i]]$Date2 - dates[[i]]$Date) > width]
        dates[[i]] <- rbind(dates[[i]], lastrow)
        currency.list[[i]] <<- dates[[i]]
    }
  }
}

################### Created for use in DateRange function ######################
CreateRange <- function(x, timeframe) {
  x + minutes(1:timeframe)
}


DateRange <- function(x, timeframe) {
      for (i in 1:length(x)) {
        dateranges[[i]] <<- lapply(x[[i]][, Date], CreateRange, timeframe = timeframe)
        }
      }

MatchRanges <- function(x) {
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]]))
    dateranges[[i]][[j]] <<- ref.list[[i]][Date %in% x[[i]][[j]]]
  }
}



TestCounter <- function(x, type = c("above", "below")) {
  if (type == "below") {
    for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])) {
      if (nrow(x[[i]][[j]][Price > mavg]) > 0) {
        success[[i]][[j]] <<- 1
      }
    }
  }
}  
  if (type == "above") {
    for (i in 1:length(x)) {
      for (j in 1:length(x[[i]])) {
        if (nrow(x[[i]][[j]][Price < mavg]) > 0) {
          success[[i]][[j]] <<- 1
        }
      }
    }
  }  
}


Calculate <- function(x) {
  for (i in 1:length(x))
    success[[i]] <<- sum(x[[i]], na.rm = TRUE) / length(x[[i]])
  
}


currency.list <- vector("list", length(currencies))
ref.list <- vector("list", length(currency.list))
dateranges <- vector("list", length(currency.list))
dates <- vector("list", length(dateranges))
success <- vector("list", length(dateranges))

MeanRevert <- function(x, sd, periods, width, timeframe, type) {
  ImportData(currencies)
  DateClean(currency.list)
  PriceClean(currency.list, sd = sd, periods = periods)
################# Algorithm splits into below and above ######################
    if (type == "below") {
      FormatDates(currency.list, type = type, width = width)
      DateRange(currency.list, timeframe = timeframe)
      MatchRanges(dateranges)
      TestCounter(dateranges, type = type)
      Calculate(success)
  }
    if (type == "above") {
      FormatDates(currency.list, type = type, width = width)
      DateRange(currency.list, timeframe = timeframe)
      MatchRanges(dateranges)
      TestCounter(dateranges, type = type)
      Calculate(success)  
  }
}


CreateDataTable <- function(Asset, scores, timeframe) {
    data.table(Currency = currencies, Score = unlist(scores), Timeframe = timeframe)
}

Outcomes <- CreateDataTable(Asset = currencies, scores = success, timeframe = 30)


ggplot(Outcomes, aes(x = Currency, y = Score, fill = Currency)) + geom_bar(stat = "identity")
