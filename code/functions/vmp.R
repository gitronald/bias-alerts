ci <- function(p, n) {
  # 95% Confidence Interval for Proportions
  #
  # Args:
  #   p: Sample proportion
  #   n: Sample size
  #
  absp <- abs(p)
  CImax   <- absp + 1.96 * sqrt(((absp) * (1 - absp) / n))
  CImin   <- absp - 1.96 * sqrt(((absp) * (1 - absp) / n))
  CI      <- c(CImin, CImax)
  if (p < 0) {
    CI <- CI * -1
  }
  return (CI)
}

ci2 <- function(p, n) {
  # 95% Confidence Interval for Proportions, with correction for > 100% increase
  #
  # Args:
  #   p: Sample proportion
  #   n: Sample size
  #
  if(is.nan(p)) p <- 0 # Correct for NaN proportion
  
  absp <- abs(p)
  absp.fix <- (1 - absp)
  if(absp > 1) {
    absp.fix = -1 * (1 - absp)
  }
  CImax   <- absp + 1.96 * sqrt(((absp) * absp.fix / n))
  CImin   <- absp - 1.96 * sqrt(((absp) * absp.fix / n))
  CI      <- c(CImin, CImax)
  if (p < 0) {
    # Flip order and multiply by negative
    CI  <- c(CImax, CImin)
    CI <- CI * -1
  }
  return (CI)
}

perc <- function(x, n){
  # Format proportion to percent with symbol
  # x: number for conversion
  # n: number of digits to show (even if 0)
  px <- paste0(format(round(x*100, n), nsmall = n),"%")
  return(px)
}


# VMP - with nVMP ---------------------------------------------------------

vmp <- function(x, y = NA, z = NA, neat = FALSE, group = TRUE, list = TRUE){
  # Compute VMP statistics and N's for a given group
  # 
  # Args:
  #   x: data object
  #   y: variable/column name
  #   z: demographic factor
  
  # Name study, demographic, and factor
  Study <- paste0(deparse(substitute(x)))
  Demographic <- paste0(y)
  Dgroup <- paste0(z)
  Demo.group <- paste(Demographic, Dgroup, sep = ".")
  
  # Select sample
  if (group) {
    x <- x[x["GroupNumber"] != "3", ]
  }
  
  # Subset if variable name is present
  if (!is.na(y)) {
      x <- x[x[[y]] == z, ]
  } 
  
  # If no rows in subset, return 0's instead of error message
  if (nrow(x) == 0) {
    data <- data.frame(Study, Demographic, Dgroup, Demo.group, 
                       N=0, Npost=0, Npre=0, Nshift=0,
                       VMP=0, CImin=0, CImax=0, 
                       Chi=0, df=0, P=0,
                       Nshift.p=0, pVMP=0, pCImin=0, pCImax=0,
                       Nshift.n=0, nVMP=0, nCImin=0, nCImax=0)
    return(data)
  }
  
  # Note: Operating on vmp.table with indices to 
  # generalize function to other variable names

  # VMP - Table
  vmp.table <- xtabs(~ x[["PreVote"]] + x[["PostVote"]])
  vmp.table <- addmargins(as.table(vmp.table))
  N      <- vmp.table["Sum", "Sum"]
  Npre   <- nrow(subset(x, PreVote == 1))
  Npost  <- nrow(subset(x, PostVote == 1))
  
  if (length(vmp.table) == 9) {
    Nshift.p <- vmp.table["0", "1"]
    Nshift.n <- vmp.table["1", "0"]
    Nshift <- Nshift.p - Nshift.n
  } else {
    Nshift.p <- 0
    Nshift.n <- 0
    Nshift <- Npost - Npre
  }
  
  # VMP - Overall Percent Difference (Percent increase/decrease)
  VMP    <- Nshift/ Npre
  CI    <- ci2(VMP, Npre)
  CImin <- CI[1]
  CImax <- CI[2]
  McNemar   <- mcnemar.test(vmp.table[-3, -3])
  Chi <- McNemar[["statistic"]]
  df <- McNemar[["parameter"]]
  P <- McNemar[["p.value"]]
  
  #pVMP & nVMP - Percent change from treatment (positive/negative)
  pVMP <- Nshift.p / N
  pCI <- ci(pVMP, N)
  pCImin <- pCI[1]
  pCImax <- pCI[2]
  nVMP <- (Nshift.n / N) * -1
  nCI <- ci(nVMP, N)
  nCImin <- nCI[1]
  nCImax <- nCI[2]
  
  # Aggregate data
  data <- data.frame(Study, Demographic, Dgroup, Demo.group,
                     N, Npost, Npre, Nshift,
                     VMP, CImin, CImax, 
                     Chi, df, P,
                     Nshift.p, pVMP, pCImin, pCImax,
                     Nshift.n, nVMP, nCImin, nCImax)
  row.names(data) <- NULL
  
  # Round numbers and add percentage signs
  if (neat){
    percents <- c("VMP", "CImin", "CImax", "pVMP", "pCImin", "pCImax", "nVMP", "nCImin", "nCImax")
    data[percents] <- apply(data[percents], 2, function(x) perc(x, 1))
    data["Chi"] <- format(round(data["Chi"], 3), nsmall = 3)
    data["P"] <- format(round(data["P"], 4), nsmall = 4)
    if (grepl("e", data[, "P"])) {
      data[, "P"] <- "0.0000"
    }
  }
  

  
  dataList <- list(data = data,
                   vmp.table = vmp.table)
  if (list){
    return(dataList)
  } else {
    return(data)
  }
}


vmp.data <- function(x, y, group = T, neat = T) {
  # Calculates vmp statistics for factors in variable y of data x
  # Excludes vmp.table from vmp function
  # x: Data object
  # y: Variable/Column name
  
  levels <- levels(as.factor(x[[y]]))
  vmp.data <- data.frame()
  for (i in 1:length(levels)) {
    name <- paste0(levels[i])
    newData <- vmp(x, y, name, group = group, neat = neat, list = F)
    vmp.data <- rbind(vmp.data, newData)
  }
  vmp.data[["Study"]] <- paste0(deparse(substitute(x)))
  rm(newData)
  # Round numbers and add percentage signs

  return(vmp.data)
}

VmpDemog <- function(x) {
  # Creates table of VMPs for demographic groups named in x
  #
  #  Args:
  #    x: vector of demographic names
  #
  DF <- data.frame()
  for (i in 1:length(x)) {
    y <- levels(mydata[[x[i]]])
    for (j in 1:length(y)) {
      df <- Vmp(mydata, x[i], y[j])
      DF <- rbind(DF, df)
    }
  }
  return (DF)
}
