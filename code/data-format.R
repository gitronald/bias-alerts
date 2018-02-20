# Format cleaned data

# Load data
study4 = read.table('data/bias_alerts_clean.tsv', sep='\t', header=T)

# Define groups
group1 <- subset(study4, GroupNumber == 1)
group2 <- subset(study4, GroupNumber == 2)
group3 <- subset(study4, GroupNumber == 3)

# Convert Pre/PostTestVote to numeric for each Group
group1[["PreVote"]]  <- ifelse(group1[["PreTestVoteWhichCandidate"]] == "David Cameron", 1, 0)
group1[["PostVote"]] <- ifelse(group1[["PostTestVoteWhichCandidate"]] == "David Cameron", 1, 0)
group2[["PreVote"]]  <- ifelse(group2[["PreTestVoteWhichCandidate"]] == "Ed Miliband", 1, 0)
group2[["PostVote"]] <- ifelse(group2[["PostTestVoteWhichCandidate"]] == "Ed Miliband", 1, 0)
group3[["PreVote"]]  <- ifelse(group3[["PreTestVoteWhichCandidate"]] == "Ed Miliband", 1, 0)
group3[["PostVote"]] <- ifelse(group3[["PostTestVoteWhichCandidate"]] == "Ed Miliband", 1, 0)

# Reattach data with new variables
study4 <- rbind(group1, group2, group3)
rm(group1, group2, group3)

# Candidate Order Correction (Horizontal Counterbalancing)
# Put LikelyVote scale on -5 Ed Miliband ... 0 ... +5 David Cameron scale
# i.e. Make Likely Vote negative for CandidateOrder==1

# Step 1: Isolate CandidateOrders
order1 <- subset(study4, CandidateOrder == 1)
order2 <- subset(study4, CandidateOrder == 2)
# Step 2: Apply correction to one order
order1$PreTestLikelyVoteBipolar = -order1$PreTestLikelyVoteBipolar 
order1$PostTestLikelyVoteBipolar = -order1$PostTestLikelyVoteBipolar

# Reattach data with CandidateOrder correction
study4 <- rbind(order1, order2)
rm(order1, order2)

# Compute VoteShift and Dif variables
study4$VoteShift          = study4$PostVote - study4$PreVote
study4$DifLikelyVote      = study4$PostTestLikelyVoteBipolar - study4$PreTestLikelyVoteBipolar
study4$DifTrustCand1      = study4$PostTestTrustCandidate1 - study4$PreTestTrustCandidate1
study4$DifLikeableCand1   = study4$PostTestLikableCandidate1 - study4$PreTestLikableCandidate1
study4$DifImpressionCand1 = study4$PostTestImpressionCandidate1 - study4$PreTestImpressionCandidate1
study4$DifTrustCand2      = study4$PostTestTrustCandidate2 - study4$PreTestTrustCandidate2
study4$DifLikeableCand2   = study4$PostTestLikableCandidate2 - study4$PreTestLikableCandidate2
study4$DifImpressionCand2 = study4$PostTestImpressionCandidate2 - study4$PreTestImpressionCandidate2


# DEMOGRAPHICS ------------------------------------------------------------

# Order experiments as factor var
study4[["ExpName"]] <- factor(study4[["ExpName"]], 
                              levels = c("Original", "BiasAlert1", "BiasAlert2"),
                              ordered = TRUE)

# Obtain demographic variable names

# Demographics 
# c("Age", "Education", 
#   "Religion", "Ethnicity", "Income", "Marital", "Birth", "Employed", 
#   "Gender", "SearchesPerWeek", "EverSearched", "LastElection", 
#   "PoliticalParty", "OtherPoliticalParty", 
#   "PoliticalView", "FamiliarCandidate1", "FamiliarCandidate2")

# Numeric
# c("NumberOfClicks", "AvgSecPerPage", "TotalSearchTime", "Country", 
#   "Fluency")

# Age - Split continuous variable into factor bins
study4$AgeFactors <- cut(study4$Age, c(18, 24, 44, 64, Inf), 
                               include.lowest = TRUE, 
                               labels = c("18-24", "25-44", "45-64", "65+"), 
                               ordered_result = TRUE)

# Gender
dput(levels(droplevels(study4[["Gender"]]))) # Obtain factor levels
GenderLevels <- c("Male", "Female") # Manual rearrange
study4[["Gender"]] <- factor(study4[["Gender"]], levels = GenderLevels)

# Ethnicity
dput(levels(study4[["Ethnicity"]])) # Obtain factor levels
EthnicityLevels <- c("White", "Black", "Hispanic", "Asian", "Mixed", "Other") # Manual rearrange
study4[["Ethnicity"]] <- factor(study4[["Ethnicity"]], levels = EthnicityLevels)

# Education
dput(levels(study4[["Education"]])) # Obtain factor levels
EducationLevels <- c("None", "Primary", "Secondary", "Bachelors", "Masters", "Doctorate") # Manual rearrange
study4[["Education"]] <- factor(study4[["Education"]], levels = EducationLevels, ordered = TRUE)

# Income
study4[["Income"]] <- gsub("^.*\\(|\\)", "", study4[["Income"]]) # Clean factor labels
study4[["Income"]] <- factor(study4[["Income"]])
dput(levels(study4[["Income"]]))
IncomeLevels <- c("Under $7,500", "$7,500 to 15,000", "$15,000 to 23,000","$23,000 to 30,000",  
                  "$30,000 to 45,000", "$45,000 to 60,000", "$60,000 to 75,000",  "$75,000 to 100,000", 
                  "$100,000 to 150,000",  "$150,000 and over", "I prefer not to say")
study4[["Income"]] <- factor(study4[["Income"]], levels = IncomeLevels, ordered = TRUE)
study4[["IncomeComb"]] <- study4[["Income"]]
levels(study4[["IncomeComb"]]) <- c("Under 15,000", "Under 15,000", "$15,000 to 30,000","$15,000 to 30,000",  
                                          "$30,000 to 45,000", "$45,000 to 60,000", "$60,000 to 75,000", "$75,000 to 100,000", 
                                          "$100,000 to 150,000", "$150,000 and over", "I prefer not to say")

# Employed
dput(levels(droplevels(study4[["Employed"]]))) # Obtain factor levels
EmployedLevels <- c("Yes", "No") # Manual rearrange
study4[["Employed"]] <- factor(study4[["Employed"]], levels = EmployedLevels)

# Marital
dput(levels(droplevels(study4[["Marital"]]))) # Obtain factor levels
MaritalLevels <- c("Married", "Never married", "Separated", "Divorced", "Widowed")
study4[["Marital"]] <- factor(study4[["Marital"]], levels = MaritalLevels)

# Religion
dput(levels(study4[["Religion"]])) # Obtain factor levels
ReligionLevels <- c("Christianity", "Hinduism", "Islam", "Other", "None", "I prefer not to say") # Manual rearrange
study4[["Religion"]] <- factor(study4[["Religion"]], levels = ReligionLevels)

# Country
CountryLevels <- dput(levels(droplevels(study4[["Country"]]))) # Obtain factor levels, leave in alpha order
study4[["Country"]] <- factor(study4[["Country"]], levels = CountryLevels)

# PoliticalView
dput(levels(droplevels(study4[["PoliticalView"]])))
study4[["PoliticalView"]] <- gsub("\\s+\\(.*\\)", "", study4[["PoliticalView"]]) # Clean factor labels
PoliticalViewLevels <- c("Conservative", "Moderate", "Liberal", "Other", "None")
study4[["PoliticalView"]] <- factor(study4[["PoliticalView"]], levels = PoliticalViewLevels)

# EverSearched
study4[["EverSearched"]] <- droplevels.factor(study4[["EverSearched"]])
study4[["EverSearched"]] <- factor(study4[["EverSearched"]], levels = c("Yes", "No"))

# LastElection
study4[["LastElection"]] <- droplevels.factor(study4[["LastElection"]])
study4[["LastElection"]] <- factor(study4[["LastElection"]], levels = c("Yes", "No"))

# Familiarity
study4[["FamiliarBoth"]] <- (study4[["FamiliarCandidate1"]] + study4[["FamiliarCandidate2"]])/2

# Bothered and BotheredText
study4[["Bothered"]] <- droplevels.factor(study4[["Bothered"]])
study4[["Bothered"]] <- factor(study4[["Bothered"]], levels = c("Yes", "No"))
study4[["BotheredText"]] <- as.character(study4[["BotheredText"]])

# Dump level vars
rm(CountryLevels, EducationLevels, EmployedLevels, EthnicityLevels, GenderLevels, 
   IncomeLevels, MaritalLevels, ReligionLevels, PoliticalViewLevels)

# Define treatment groups
study4[["Treatment"]] <- ifelse(study4["GroupNumber"] == 3, FALSE, TRUE) 

# Combine Countries
study4[["CountryComb"]] <- study4[["Country"]]
levels(study4[["CountryComb"]]) <- c("Other", "Other", "Other", "Other", "Other", 
                                     "Other", "Other", "Other", "Other", "Other", 
                                     "Other", "Other", "Other", "India", "Other", "Other", 
                                     "Other", "Other", "Other", "Other", "Other", 
                                     "Other", "Other", "Other", "Other", "Other", "Other", 
                                     "Other", "Other", "Other", "Other", "Other", 
                                     "Other", "Other", "Other", "United Kingdom", "United Kingdom", 
                                     "United States", "Other", "Other", "Other")


# DATA - Webpage Timers & Clicks ------------------------------------------

# Isolate groups for ordering
g1 <- study4[study4["GroupNumber"] == 1, ]
g2 <- study4[study4["GroupNumber"] == 2, ]
g3 <- study4[study4["GroupNumber"] == 3, ]

# Read in search orders file
#  Each row represents a group - 1, 2, 3 - and each column contains
#  the webpage number that appeared at that ranking.
so <- read.table("data/Search_Order_by_Group_Database.tsv", 
                 header=FALSE, 
                 sep="\t")

# Bind an index (numeric 1-30 key) to webpage number
so1 <- rbind(so[1, ], 1:30)
so2 <- rbind(so[2, ], 1:30)
so3 <- rbind(so[3, ], 1:30)

# Sort webpage numbers by search order
so1 <- so1[order(so1[1, ])]
so2 <- so2[order(so2[1, ])]
so3 <- so3[order(so3[1, ])]

### TIMERS

# Rename webpage timers by sorted webpage numbers
for (i in 1:30){
  names(g1)[48+i] <- paste0("Timer", so1[2,i])   #Group 1
}
for (i in 1:30){
  names(g2)[48+i] <- paste0("Timer", so2[2,i])   #Group 2
}
for (i in 1:30){
  names(g3)[48+i] <- paste0("Timer", so3[2,i])   #Group 3
}

# Bind index numbers to sorted webpage numbers
so1.timers <- rbind(so1, 49:78)
so2.timers <- rbind(so2, 49:78)
so3.timers <- rbind(so3, 49:78)

# Sort index numbers by sorted webpage numbers
so1.timers <- so1.timers[order(so1.timers[2,])]
so2.timers <- so2.timers[order(so2.timers[2,])]
so3.timers <- so3.timers[order(so3.timers[2,])]

# Reorder webpage timers column names
g1 <- g1[, c(1:48, dput(as.numeric(so1.timers[3, ])), 79:ncol(g1))]
g2 <- g2[, c(1:48, dput(as.numeric(so2.timers[3, ])), 79:ncol(g2))]
g3 <- g3[, c(1:48, dput(as.numeric(so3.timers[3, ])), 79:ncol(g3))]


#### CLICKS

# Rename webpage clicks by sorted webpage numbers
for (i in 1:30){
  names(g1)[78+i] <- paste0("Clicks", so1[2,i])   #Group 1
}
for (i in 1:30){
  names(g2)[78+i] <- paste0("Clicks", so2[2,i])   #Group 2
}
for (i in 1:30){
  names(g3)[78+i] <- paste0("Clicks", so3[2,i])   #Group 3
}

# Bind index numbers to sorted webpage numbers
so1.clicks <- rbind(so1, 79:108)
so2.clicks <- rbind(so2, 79:108)
so3.clicks <- rbind(so3, 79:108)

# Sort index numbers by sorted webpage numbers
so1.clicks <- so1.clicks[order(so1.clicks[2,])]
so2.clicks <- so2.clicks[order(so2.clicks[2,])]
so3.clicks <- so3.clicks[order(so3.clicks[2,])]

# Reorder webpage clicks column names
g1 <- g1[, c(1:78, dput(as.numeric(so1.clicks[3, ])), 109:ncol(g1))]
g2 <- g2[, c(1:78, dput(as.numeric(so2.clicks[3, ])), 109:ncol(g2))]
g3 <- g3[, c(1:78, dput(as.numeric(so3.clicks[3, ])), 109:ncol(g3))]

# Recombine Data
study4 <- rbind(g1, g2, g3)

# Remove process data
rm(g1, g2, g3, so, i,
   so1, so1.clicks, so1.timers, 
   so2, so2.clicks, so2.timers, 
   so3, so3.clicks, so3.timers)

# Write out formatted data as Rdata file to maintain formatting
save(study4, file="data/bias_alerts_format.Rda")
