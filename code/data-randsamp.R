# Take a random sample of cleaned and formatted data

# Load formatted data
load("data/bias_alerts_format.Rda")

source("code/functions/rand_samp.R")

split <- split(study4, study4$ExpName)
appld <- lapply(split, rand_samp, "GroupNumber", "CandidateOrder", 200)
study4 <- do.call(rbind, appld)

# Check group and order balances
lapply(unique(study4$ExpName), function(x) xtabs(~GroupNumber+CandidateOrder, study4[study4$ExpName==x, ]))

# CAUTION: RUNNING THIS LINE WILL OVERWRITE EXISTING RANDOM SAMPLE THAT WAS USED IN PUBLISHED PAPER
#save(study4, file = "bias_alerts.Rda")
