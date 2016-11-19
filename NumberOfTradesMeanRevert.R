test <- dateranges[[1]]

loc <- vector("numeric", length = 0)
for (i in 1:(length(test) - 1)) {
    last.date <- test[[i]][.N][, Date.Time]
    first.date <- test[[(i + 1)]][, Date.Time][1]
    revert <- test[[i]][Price > mavg[1]][, Date.Time][1]
    if (!is.na(revert)) {
      if (last.date > first.date & revert > first.date) {
        loc <- c(loc, (i +1))
    } else if (last.date > first.date & revert < first.date) { 
        next
    } 
  
  } else if (last.date > first.date) {
      loc <- c(loc, (i + 1))
  } else {
      next
  }
}




test[[3]][Price > mavg[1]][, Date.Time][1] > test[[4]][, Date.Time][1]

} else if ((last.date < first.date) == TRUE) {
  next
} else if (((last.date > first.date) & (revert < first.date)) {
  next
}