# DATA - Load raw ---------------------------------------------------------

# Raw data
study4 <- read.table("data/bias_alerts_raw.tsv", sep = "\t", header = TRUE)

# DATA - Clean ------------------------------------------------------------

# Delete corrupt and missing data
study4 <- subset(study4, !is.na(study4$FamiliarCandidate1))

# Delete timer errors
#  Check if TimePage[1:5] + Timer[1:30] == TotalSearchTime
#  Delete cases where above is FALSE 
TimerCheck <- rep(NA, nrow(study4))
for (i in 1:nrow(study4)){
  TimerCheck[i] <- identical(sum(study4[i, 44:78]), sum(study4[i, "TotalSearchTime"]))
}
study4 <- study4[TimerCheck, ]
rm(TimerCheck, i)

# Examine fluency and drop subjects 5 & under
table(study4$Fluency)
study4 <- subset(study4, Fluency > 5)

# Check group and order balances
lapply(unique(study4$ExpName), function(x) xtabs(~GroupNumber+CandidateOrder, study4[study4$ExpName==x, ]))


# Cleaned Data ------------------------------------------------------------

# Write out cleaned data
write.table(study4, "data/bias_alerts_clean.tsv", sep = "\t", row.names = F)