# Program that updates a designated set of currencies 
library(data.table)


currencies <- c("AUDUSD.csv", "AUDJPY.csv", 
                "EURUSD.csv", "EURCHF.csv", "EURGBP.csv", "EURJPY.csv",
                "GBPJPY.csv", "GBPUSD.csv",
                "NZDUSD.csv",
                "USDCAD.csv", "USDJPY.csv", "USDCHF.csv", "USDCNH.csv")

currencies.new <- c("AUDUSDUpdate.csv", "AUDJPYUpdate.csv", 
                    "EURUSDUpdate.csv", "EURCHFUpdate.csv", "EURGBPUpdate.csv", "EURJPYUpdate.csv",
                    "GBPJPYUpdate.csv", "GBPUSDUpdate.csv",
                    "NZDUSDUpdate.csv",
                    "USDCADUpdate.csv", "USDJPYUpdate.csv", "USDCHFUpdate.csv", "USDCNHUpdate.csv")


setwd("C:/Users/David/Desktop/Currency/CurrencyFinals")
############################### Function to import data ########################
currency.list <- vector("list", length = length(currencies))
for (i in 1:length(currencies)) {
  currency <- fread(currencies[i], header = FALSE, select = c(2, 3, 4),
                      col.names = c("Date.Time", "Close.Ask", "Close.Bid"),
                      skip = 1)
  tmp <- data.table(currency)
  currency.list[[i]] <- tmp # fill in currency.list element i 
  currency.list[[i]] <- currency.list[[i]][order(Date.Time)] # order dates

}

setwd("C:/Users/David/Desktop/Currency/CurrencyUpdate/csvs")
# Loop over `currencies.new` and read1 in the data associated with each currency
currency.list.new <- vector("list", length = length(currencies.new))
for (i in 1:length(currencies.new)) {
  currency <- fread(currencies.new[i], header = FALSE, select = c(1, 5, 9), 
                      col.names = c("Date.Time", "Close.Ask", "Close.Bid"),
                      skip = 1)
  tmp <- data.table(currency)
  currency.list.new[[i]] <- tmp
  currency.list.new[[i]] <- currency.list.new[[i]][order(Date.Time)]
}


# Now we need to find the values that are different from each currency list
for (i in 1:length(currency.list)) {
  dates.old <- currency.list[[i]][, Date.Time]  
  new.data <- currency.list.new[[i]][!(Date.Time %in% dates.old)] 
  currency.list[[i]] <- rbind(currency.list[[i]], new.data)
  currency.list[[i]] <- currency.list[[i]][order(Date.Time)]
}


# Now we need to save the new data files to their original files
names(currency.list) <- currencies

setwd("C:/Users/David/Desktop/Currency/CurrencyFinals")
for (i in 1:length(currency.list)) {
  write.csv(currency.list[[i]], file = names(currency.list)[i])
}


