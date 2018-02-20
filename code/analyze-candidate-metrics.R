# Candidate ratings and preferences tables

# Load functions from /R
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files) 

# Load data
load("data/bias_alerts.Rda")

og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

# Between Subjects Tests ---------------------------------------------------

# Statistical testing between group 1, 2, and 3 pre and post search
# using nonparametric statistics and Bonferroni p-value corrections 
# for multiple hypothesis testing.

# Candidate Ratings ------------------------------------------------

### Filter for ratings variables
measures = names(og)[which(grepl('Pre|Post', names(og)))]
measures = measures[which(grepl('Candidate1|Candidate2', measures))]
measures = measures[which(!grepl('Likely', measures))]

### Get statistics
candidate_ratings = rbind(between_group_testing(og, measures),
                          between_group_testing(b1, measures),
                          between_group_testing(b2, measures))

### Format and reorder variables
candidate_ratings['variable'] = gsub('Test', '', candidate_ratings$variable)
candidate_ratings['variable'] = gsub('Candidate1', 'Cameron', candidate_ratings$variable)
candidate_ratings['variable'] = gsub('Candidate2', 'Milliband', candidate_ratings$variable)

candidate_ratings$experiment = c("No Alert", rep("", (nrow(candidate_ratings)/3 - 1) ),
                                 "Low Alert", rep("", (nrow(candidate_ratings)/3 - 1) ),
                                 "High Alert", rep("", (nrow(candidate_ratings)/3 - 1) ))

cols = c('kw_df', 'kw_pval', 'kw_padj', 'mw_pval', 'mw_padj')
candidate_ratings[, cols] = list(NULL)
candidate_ratings = cbind(candidate_ratings[1:5], candidate_ratings[6], candidate_ratings[8], candidate_ratings[7], candidate_ratings[9])

# Latex Madness -----------------------------------------------------------

### LaTeX Table:
library(xtable)

#### Prepare column names for print
colnames(candidate_ratings) = c("Experiment", "Measure", "Group 1", "Group 2", "Group 3", "K-W $\\chi^2$", 'kw_sig', "M-W $U$", 'mw_sig')

#### Create xtable object
xt = xtable(candidate_ratings, 
            align='lllrrrrlrl', 
            caption= c("Candidate ratings by experiment"))

#### Capture print with options
# To use add.to.row you must add the below to your Latex document:
# \usepackage{array}
# \newcolumntype{L}[1]{>{\raggedright\arraybackslash}m{#1}}
#
latex = capture.output(print(xt, 
                             include.rownames=FALSE, 
                             sanitize.colnames.function=identity,
                             caption.placement = 'top',
                             add.to.row = list(list(nrow(candidate_ratings)),  
                                               "\\hline  \\multicolumn{7}{L{16cm}}{\\textbf{Note: } 
                                               Kruskal Wallis (K-W) tests were conducted between all three groups 
                                               and had two degrees of freedom. All Mann-Whitney (M-W) tests were
                                               conducted between the two bias groups, group one and group two.} \\\\")
                             ))

#### Remove extra shit
latex = gsub("\\~{}", "", latex, fixed = T)

cat(latex, sep='\n', file = 'tables/candidate-ratings.tex')


# Candidate Preferences ---------------------------------------------------

# Get statistics
measures = c("PreTestLikelyVoteBipolar", "PostTestLikelyVoteBipolar")

candidate_preferences = rbind(between_group_testing(og, measures),
                              between_group_testing(b1, measures),
                              between_group_testing(b2, measures))

### Format data
candidate_preferences['variable'] = gsub('Test', '', candidate_preferences$variable)
candidate_preferences['variable'] = gsub('Candidate1', 'Cameron', candidate_preferences$variable)
candidate_preferences['variable'] = gsub('Candidate2', 'Milliband', candidate_preferences$variable)

candidate_preferences$experiment = c("No Alert", rep("", (nrow(candidate_preferences)/3 - 1) ),
                                     "Low Alert", rep("", (nrow(candidate_preferences)/3 - 1) ),
                                     "High Alert", rep("", (nrow(candidate_preferences)/3 - 1) ))

cols = c('kw_df', 'kw_pval', 'kw_padj', 'mw_pval', 'mw_padj')
candidate_preferences[, cols] = list(NULL)
candidate_preferences = cbind(candidate_preferences[1:5], candidate_preferences[6], candidate_preferences[8], candidate_preferences[7], candidate_preferences[9])

# Latex Madness -----------------------------------------------------------

### LaTeX Table:
library(xtable)

#### Prepare column names for print
colnames(candidate_preferences) = c("Experiment", "Measure", "Group 1", "Group 2", "Group 3", "K-W $\\chi^2$", "M-W $U$")

#### Create xtable object
xt = xtable(candidate_preferences, 
            align='lllrrrrlrl', 
            caption= c("Candidate preferences by experiment"))

#### Capture print with options
# To use add.to.row you must add the below to your Latex document:
# \usepackage{array}
# \newcolumntype{L}[1]{>{\raggedright\arraybackslash}m{#1}}
#
latex = capture.output(print(xt, 
                             include.rownames=FALSE, 
                             sanitize.colnames.function=identity,
                             caption.placement = 'top',
                             add.to.row = list(list(nrow(candidate_preferences)),  
                                               "\\hline  \\multicolumn{7}{L{16cm}}{\\textbf{Note: } 
                                               Kruskal Wallis (K-W) tests were conducted between all three groups 
                                               and had two degrees of freedom. All Mann-Whitney (M-W) tests were
                                               conducted between the two bias groups, group one and group two.} \\\\")
                             ))

cat(latex, sep='\n', file = 'tables/candidate-preferences.tex')


# Within Group Comparisons ------------------------------------------------

# Define differnce variables
DifVariables <- c("DifLikelyVote", 
                  "DifTrustCand1", "DifLikeableCand1", "DifImpressionCand1", 
                  "DifTrustCand2", "DifLikeableCand2", "DifImpressionCand2")

# Original control comparisons
og.mw.compare <- rbind(
  do.call(rbind.data.frame, lapply(DifVariables[1:4], mw.compare, data = og, groupA = 1, groupB = 3, alt = "greater")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = og, groupA = 1, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[1:4], mw.compare, data = og, groupA = 2, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = og, groupA = 2, groupB = 3, alt = "greater"))
)

# BiasAlert1 control comparisons
b1.mw.compare <- rbind(
  do.call(rbind.data.frame, lapply(DifVariables[1:4], mw.compare, data = b1, groupA = 1, groupB = 3, alt = "greater")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = b1, groupA = 1, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[1:4], mw.compare, data = b1, groupA = 2, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = b1, groupA = 2, groupB = 3, alt = "greater"))
)

# BiasAlert2 control comparisons
b2.mw.compare <- rbind(
  do.call(rbind.data.frame,lapply(DifVariables[1:4], mw.compare, data = b2, groupA = 1, groupB = 3, alt = "greater")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = b2, groupA = 1, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[1:4], mw.compare, data = b2, groupA = 2, groupB = 3, alt = "less")),
  do.call(rbind.data.frame, lapply(DifVariables[5:7], mw.compare, data = b2, groupA = 2, groupB = 3, alt = "greater"))
)

### Split results
s4.prefs   <- rbind(og.mw.compare, b1.mw.compare, b2.mw.compare)
s4.bipolar <- s4.prefs[s4.prefs["variable"] == "DifLikelyVote", ]
s4.prefs   <- s4.prefs[s4.prefs["variable"] != "DifLikelyVote", ]

### Generate and add indices
Candidate <- rep(c("Cameron","Miliband"), 6, each = 3)
Rating <- rep(c("Trust", "Like", "Impression"), 12)
s4.prefs <- data.frame(Candidate = Candidate, Rating = Rating, s4.prefs)
s4.prefs$experiment <- factor(s4.prefs$experiment,
                              labels = c("No Alert", "Low Alert", "High Alert"))

### Paneled graph of voting preferences by experiment
p = ggplot(s4.prefs, aes(y=mean, x=variable, colour=groupA, shape=groupA)) +
  geom_pointrange(aes(ymin=mean-se*1.96, ymax=mean+se*1.96), size = 0.55) +
  coord_flip() +
  facet_grid(experiment ~ .) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_x_discrete(limits = rev(levels(s4.prefs$variable)[-1]),
                   labels = c("Impression M", "Like M", "Trust M",
                              "Impression C", "Like C", "Trust C")) +
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, 0.5), expand = c(0,0)) +
  scale_shape_manual(values = c(19, 17), labels = c("Cameron Bias", "Miliband Bias")) +
  scale_color_manual(values =  c("#5B9BD5", "#C00000"), labels = c("Cameron Bias", "Miliband Bias")) +
  labs(x = NULL, 
       y = "Mean Shift from Control Group Ratings") +
  # title = "Shifts in Candidate Ratings by Group and Experiment", 
  theme_bw(base_size = 7) +
  theme(title = element_text(size = 8, vjust = 1.5),
        axis.title.x = element_text(size = 8, vjust = -0.5, colour = "#444444"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = c(0.82,0.095),
        legend.title = element_blank(),
        legend.key.size = unit(0.7, 'line'),
        legend.text = element_text(size = 8),
        strip.text = element_text(size = 9),
        legend.key = element_rect(colour = NA),
        legend.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14))

p = p + geom_vline(aes(xintercept = 3.5), colour ="gray")

### Save image
pdf('plots/prepost-cand-ratings.pdf', 3.6, 3.6)
print(p)
dev.off()

### Paneled graph of bipolar measure by experiment
s4.bipolar$experiment <- factor(s4.bipolar$experiment,
                                labels = addline_format(c("No Alert", "Low Alert", "High Alert")))

p2 = ggplot(s4.bipolar, aes(y=mean, x=variable, colour=groupA, shape=groupA)) +
  geom_pointrange(aes(ymin=mean-se*1.96, ymax=mean+se*1.96), size = 1) +
  coord_flip() +
  facet_grid(experiment ~ .) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5), expand = c(0,0)) +
  scale_shape_manual(values = c(19, 17), labels = c("Cameron Bias", "Miliband Bias")) +
  scale_color_manual(values =  c("#5B9BD5", "#C00000"), labels = c("Cameron Bias", "Miliband Bias")) +
  labs(x = NULL, 
       y = "Mean Shift from Control Group Ratings") +
  #title = "Shifts in Voting Preferences by Group and Experiment", 
  theme_bw(base_size = 16) +
  theme(title = element_text(vjust = 1.5),
        axis.title.x = element_text(size = 16, vjust = -0.5), #, colour = "#7F7F7F"
        legend.position = c(0.5,0.85),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.key = element_rect(colour = NA),
        legend.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 14))

### Save image
pdf('plots/ByExperiment-CandidatePreferences.pdf', 7, 3.0)
print(p2)
dev.off()

# Candidate Preferences Significance ------------------------------------------------

# Which shifts were not significantly different from control?
s4.prefs[which(s4.prefs$p > 0.05), ]

og.biasg = subset(og, Treatment == T)
b1.biasg = subset(b1, Treatment == T)
b2.biasg = subset(b2, Treatment == T)

# No Alert
kruskal.test(og$PreTestLikelyVoteBipolar, og$GroupNumber)
kruskal.test(og$PostTestLikelyVoteBipolar, og$GroupNumber)
wilcox.test(subset(og.biasg, GroupNumber == 1)$PostTestLikelyVoteBipolar, 
            subset(og.biasg, GroupNumber == 2)$PostTestLikelyVoteBipolar)

# Low Alert
kruskal.test(b1$PreTestLikelyVoteBipolar, b1$GroupNumber)
kruskal.test(b1$PostTestLikelyVoteBipolar, b1$GroupNumber)
wilcox.test(subset(b1.biasg, GroupNumber == 1)$PostTestLikelyVoteBipolar, 
            subset(b1.biasg, GroupNumber == 2)$PostTestLikelyVoteBipolar)

# High Alert
kruskal.test(b2$PreTestLikelyVoteBipolar, b2$GroupNumber)
kruskal.test(b2$PostTestLikelyVoteBipolar, b2$GroupNumber)
wilcox.test(subset(b2.biasg, GroupNumber == 1)$PostTestLikelyVoteBipolar, 
            subset(b2.biasg, GroupNumber == 2)$PostTestLikelyVoteBipolar)

wilcox.test(subset(og, GroupNumber == 1)$DifLikelyVote, 
            subset(b1, GroupNumber == 1)$DifLikelyVote)
wilcox.test(subset(og, GroupNumber == 2)$DifLikelyVote, 
            subset(b1, GroupNumber == 2)$DifLikelyVote)

wilcox.test(subset(b1, GroupNumber == 1)$DifLikelyVote, 
            subset(b2, GroupNumber == 1)$DifLikelyVote)
wilcox.test(subset(b1, GroupNumber == 2)$DifLikelyVote, 
            subset(b2, GroupNumber == 2)$DifLikelyVote)

wilcox.test(subset(og, GroupNumber == 1)$DifLikelyVote, 
            subset(b2, GroupNumber == 1)$DifLikelyVote)
wilcox.test(subset(og, GroupNumber == 2)$DifLikelyVote, 
            subset(b2, GroupNumber == 2)$DifLikelyVote)

