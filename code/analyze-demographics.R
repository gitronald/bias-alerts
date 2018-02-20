# Demographics table

library(dtables)

# Load functions
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)

# Load data
load("data/bias_alerts.Rda")

# Demographics Table ------------------------------------------------------

dvars = c("Gender", "Education", "Ethnicity", "IncomeComb", 
          "Marital", "Employed", "PoliticalView", "Religion",
          "CountryComb")
demographics = dtable(study4, dvars)
demographics = demographics$Frequencies[, 2:5]

# Edit variable names
demographics$demographic = gsub('Comb', '', demographics$demographic)
demographics$demographic = gsub('PoliticalView', 'Political view', demographics$demographic)

col_names = c(" ", "Group", "\\$n\\$", "%")
CAPTION  = "Demographics for the three experiments in aggregate"
TAB = deparse(substitute(demographics))
SAVE_AS = paste0('tables/', TAB, '.tex')

latex_table(demographics, col_names, align='lllrr', CAPTION, TAB, SAVE_AS)
