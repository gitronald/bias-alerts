# Replicate all results

source('code/data-clean.R')
source('code/data-format.R')
source('code/data-randsamp.R')
source('code/analyze-demographics.R')
source('code/analyze-search-metrics.R', print.eval = T)
source('code/analyze-candidate-metrics.R', print.eval = T)
source('code/analyze-vmp.R', print.eval = T)
source('code/analyze-vote-shift.R', print.eval = T)
source('code/analyze-vmp-familiarity.R', print.eval = T)
source('code/analyze-vmp-demographics.R', print.eval = T)
source('code/analyze-vmp-behavior-attitude.R', print.eval = T)
source('code/analyze-bias-awareness.R', print.eval = T)