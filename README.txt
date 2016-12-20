This repository is for a project I've been working on related to currency pair trading. In this repo you'll find
a few algorithms and some data that can be used to reproduce my code. Here is a list of each algorithm and
what it does:

MeanRevertingAlgo.R:

The inital algo I made to determine how many times a time series reverts to a moving average. The algo allows
the user to determine how far away the price can deviate by choosing the moving standard deviations (Bollinger
Bands - BBands) to calculate. Once this is determined, the algo selects all the data outside of these bands, 
and then counts all of the times it reverts back to the moving average at the time of deviation. 

MeanRevertingAlgoLoop.R:

This algo is the same as above, however it is geared towards working with a list of currency pair data, rather
than one currency pair. It does what the above algo does, but it loops through a list object. 

NumberOfTradesMeanRevert.R:

This is a simple algo I'm working on to help determine the actual number of trades that would be executed 
in a live implementation of this strategy. Sometimes a price deviates outside of the Bollinger Bands again 
before it reverts back to the mean. So, let's say the price deviates outside of the Bollinger Band at 8am 
and then does again at 8:10am before ever reverting back to the mean. In a live trading scenario, I wouldn't
enter the trade at 8:10am, because I am already in the trade associated with the 8am deviation. 

PriceDataUpdater.R

This is an algo that reformats some data that I pull from my trading platform and adds it to the existing 
data files I have on my local machine associated with a given currency pair. 

