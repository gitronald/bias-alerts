# Random sampling for POS experiments
# Takes in dataset "x", calculates smallest group/order group and takes
# a random sample based on that number from each group/order combination.
# If n is set, tries to take n from each group/order combo.

rand_samp <- function(x, var1, var2, n = NULL) {
  min = n
  if (is.null(min)){
    index <- dtables::data_frame_table(x[, c(var1, var2)])
    min   <- index[which.min(index$Freq), "Freq"]
  }
  y <- data.frame()
  for(i in 1:3){
    for(j in 1:2){
      z <- x[sample(which(x[, var1] == i & x[, var2] == j), min), ]
      y <- rbind(y, z)
    }
  }
  return(y)
}
