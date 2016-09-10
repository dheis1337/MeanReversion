##### Load Packages
library(TTR)
library(data.table)
library(lubridate)
library(ggplot2)

setwd("C:/MyStuff/DataScience/Projects/MeanReversion")

##### Vector with names of data files 
currencies <- c("EURUSD.csv", "GBPUSD.csv", "USDJPY.csv", "AUDUSD.csv", "USDCHF.csv",
                "USDCAD.csv")

############################### Function to import data ########################
ImportData <- function(x) {
    # Loop over the vector with the desired currencies and create a list 
    # of data.tables for each one and reorders the data.tables in sequential order
    # by dates
    for (i in 1:length(x)) {
      currency <- fread(x[i], header = FALSE, select = c(1, 5, 9),
                        col.names = c("Date.Time", "Close.Ask", "Close.Bid"),
                        skip = 1)
      tmp <- data.table(currency)
      currency.list[[i]] <<- tmp # fill in currency.list element i 
      currency.list[[i]] <<- currency.list[[i]][order(Date.Time)] # order dates
    }
}
################################################################################

################### Function to clean to date format of each currency ##########
DateClean <- function(x) {
    # Takes currency.list created from ImportData function and converts Date 
    # variable to a POSIXct class
    for (i in 1:length(x)) {
      x[[i]][, Date.Time := gsub("/", "-", x[[i]]$Date.Time)] # Replaces '/' with '-'
      x[[i]][, Date.Time := parse_date_time(x[[i]]$Date.Time, orders = c("%m-%d-%Y %H:%M:%S"),
                                       tz = "America/New_York")]
    }
}
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
    ref.list[[i]] <<- x[[i]] # All original data; used for a later referencing
  }
  
}

############################ Function to reformat dates ########################
FormatDates <- function (x, type, width) {
    # Function takes a list and loops through each element of the list. In this 
    # case, each element is a data.table with a different currency. The 'type' 
    # arugment sets which type of band you want to look at, e.g. upper or lower 
    # band. The 'dates' variable is a list that is created as a dummy list, and 
    # populated with all of the days where the condition Pice < dn, or Price > up 
    # (depending on 'type' argument). The Date.Time2 column is created by removing the 
    # first elemnt of Date.Time, and adding an NA value. I do this so I can move each 
    # Date entry down one and placing it into its new column Date.Time2. In doing, I can
    # now calculate Date.Time2 - Date.Time which gives me a difference expressed 
    # in the number of minutes in between line n and line n + 1. The last argument
    # of the function is 'width', which is the number of minutes you want between
    # each observation. For instance, a lot of times the n, n + 1, n + 2, n +3, etc
    # observations will be in consecutive minutes. By setting the 'width' argument
    # I can say "I want observations that are at least 5 minutes a part (assuming 
    # width = 5). By observations I am referring to times where the Price < dn or
    # Price > up, depending on the 'type'.
    if (type == "below") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price < dn] 
        dates[[i]] <- dates[[i]][, Date.Time2 := as.POSIXct(c(dates[[i]]$Date.Time[-1], NA),
                                              tz = "America/Denver")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <- dates[[i]][(dates[[i]]$Date.Time2 - dates[[i]]$Date.Time) > width]
        dates[[i]] <- rbind(dates[[i]], lastrow) # Used to get last row which is lost 
                                                 # above calculation b/c of the NA
        currency.list[[i]] <<- dates[[i]]
  }
}   else if (type == "above") {
      for (i in 1:length(x)) {
        lastrow <- data.table()
        dates[[i]] <- x[[i]][Price > up]
        dates[[i]] <- dates[[i]][, Date.Time2 := as.POSIXct(c(dates[[i]]$Date.Time[-1], NA),
                                                       tz = "America/Denver")]
        lastrow <- dates[[i]][.N]
        dates[[i]] <- dates[[i]][(dates[[i]]$Date.Time2 - dates[[i]]$Date.Time) > width]
        dates[[i]] <- rbind(dates[[i]], lastrow)
        currency.list[[i]] <<- dates[[i]]
    }
  }
}
################################################################################

################### Created for use in DateRange function ######################
CreateRange <- function(x, timeframe) {
  # Function is used in the DateRange function. 
  x + minutes(1:timeframe)
}
################################################################################


############################ Function to operate CreateRange on currency.list ##
DateRange <- function(x, timeframe) {
  # The 'x' argument is a nested list of currencies that have been passed through 
  # the FormatDates function, i.e. each observation for every currency that met 
  # the logical subsetting in FormatDates. The 'timeframe' argument is how far in 
  # the future you want to look (in minutes) to test to see if the the price reverts 
  # back to the mean.  
    for (i in 1:length(x)) {
        dateranges[[i]] <<- lapply(x[[i]][, Date.Time], CreateRange, timeframe = timeframe)
        
        }
      }
################################################################################


###################### Function to match test ranges with original data ########
MatchRanges <- function(x) {
  # Arugment 'x' is a list of currencies that have appropriate timeframes added 
  # to their time. The function matches the dates in 'x', which are the last period
  # the price is outside of the band plus the 'timeframe' used in DateRange
  for (i in 1:length(x)) {
    for (j in 1:length(x[[i]]))
    dateranges[[i]][[j]] <<- ref.list[[i]][Date.Time %in% x[[i]][[j]]]
  }
}
################################################################################

########################## Function to evaluate if price reverts ###############
TestCounter <- function(x, type = c("above", "below")) {
  # The 'x' argument is the list of currencies and their observation ranges. The 
  # type is the same as the FormatDates function, and is used to determine whether
  # to test if Price > mavg (in lower band case) or if Price < mavg (in upper band
  # case). If the price reverts, the 'success' object which is a list that has one 
  # element for each currency, is populated with a 1. This list is then used to calculate
  # our end result: Number of prices that revert after deviating / Number of price deviations
  if (type == "below") {
    for (i in 1:length(x)) {
    for (j in 1:length(x[[i]])) {
      if (nrow(x[[i]][[j]][Price > mavg[1]]) > 0) { # mavg[1] is mavg at last
                                                    # time of deviation
        success[[i]][[j]] <<- 1
      }
    }
  }
}  
  if (type == "above") {
    for (i in 1:length(x)) {
      for (j in 1:length(x[[i]])) {
        if (nrow(x[[i]][[j]][Price < mavg[1]]) > 0) {
          success[[i]][[j]] <<- 1
        }
      }
    }
  }  
}
################################################################################


######################### Function to calculate ################################
Calculate <- function(x) {
  for (i in 1:length(x))
    success[[i]] <<- sum(x[[i]], na.rm = TRUE) / length(x[[i]])
  
}
################################################################################



# After loading all appropriate functions, run this: 
currency.list <- vector("list", length(currencies))
ref.list <- vector("list", length(currency.list))
dateranges <- vector("list", length(currency.list))
dates <- vector("list", length(dateranges))
success <- vector("list", length(dateranges))

MeanRevert <- function(x, sd, periods, width, timeframe, type) {
  # The 'x' argument is the first currency.list created for each currency. The 
  # 'sd' argument is the standard deviations for the Boll Bands. The 'periods' 
  # arugment are the number of periods used in the Bool Bands/mavg calculation. 
  # The 'width', 'timeframe', and 'type' arguments are the same as in the above
  # functions. This functions combines all other functions so only one function
  # call must be made to determine the outcome for each currency.
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

#### Function call
MeanRevert(currency.list, sd = 2, periods = 30, width = 5, timeframe = 60, type = "below")

###### Function create a data.table of results #################################
CreateDataTable <- function(Asset, scores, timeframe) {
    # 'Asset' argument is the list of prices for any asset (here currencies). 
    # 'scores' argument is the 'success' vector from MeanRevert call. 
    # 'timeframe' is the same as in above MeanRevert call
    data.table(Asset = currencies, Score = unlist(scores), Timeframe = timeframe)
}

###### Creates variable from CreateDataTable function call
Outcomes <- CreateDataTable(Asset = currencies, scores = success, timeframe = 60)
Outcomes <- Outcomes[order(Score)]

##### Plots above 'Outcomes' object 
ggplot(Outcomes, aes(x = Asset, y = Score, fill = Asset)) + geom_bar(stat = "identity")

