# Vote shift by bias group and experiment

library(ggplot2)
library(dtables)

# Load functions
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)

# Load data
load("data/bias_alerts.Rda")

og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

# Vote Shift --------------------------------------------------------------

get_vote_shift = function(data1, voteshift='VoteShift', by){
  # Function for producing a Post - Pre-vote table by another variable
  # data1 : dataset
  # voteshift: vote shift variable
  # by: factor to compare vote shift by
  
  tbl = table(data1[, by], data1[, voteshift]); 
  df = data.frame(matrix(tbl, dimnames = lapply(dimnames(tbl), unlist), ncol = ncol(tbl)))
  names(df) = gsub('X|\\.', '', names(df))
  
  return(df)
}
# Shape data for plotting
plot_data = data.frame(table(study4$ExpName, study4$GroupNumber, study4$VoteShift))
plot_data$prop = plot_data$Freq / 400
plot_data = cbind(plot_data, 
      setNames(data.frame(matrix(unlist(lapply(plot_data$prop, function(x) ci2(x, 400))), ncol = 2, byrow=T)), 
               c('cimin', 'cimax')))
plot_data$Var1 = factor(plot_data$Var1, levels = c('Original', 'BiasAlert1', 'BiasAlert2'), labels = c('No Alert', 'Low Alert', 'High Alert'), ordered = T)
plot_data$Var2 = factor(plot_data$Var2, levels = c('1', '2', '3'), labels = c('Cameron Bias', 'Miliband Bias', 'Neutral'), ordered = T)
plot_data = setNames(plot_data, c('Experiment', 'Group', 'VoteShift', 'Freq', 'prop', 'cimin','cimax'))

# Rename y-axis labels
plot_data$VoteShift = gsub(-1, 'Negative', plot_data$VoteShift)
plot_data$VoteShift = gsub(1, 'Positive', plot_data$VoteShift)

# Plot vote shift data
vote_shift = ggplot(subset(plot_data, VoteShift %in% c('Positive', 'Negative')), 
       aes(x = VoteShift, y = prop, group = Group, color = Group, shape = Group)) + 
  geom_pointrange(aes(ymin = cimin, ymax = cimax), position=position_dodge(0.15), size = 0.8) + 
  facet_grid(Experiment~.) + 
  scale_y_continuous(name = 'Percent Change', 
                     labels = scales::percent_format(), 
                     limits = c(0, 0.3),
                     expand = c(0,0),
                     breaks = seq(0, 0.3, 0.1)) +
  coord_flip() + 
  labs(x='Vote Shift') +
  theme_bw(base_size = 12) + 
  theme(legend.position = c(0.82,0.18),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key = element_rect(colour = NA),
        legend.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_color_manual(values = c("#5B9BD5", "#C00000", '#458B00'))

# Save Figure 5
pdf("plots/vote-shift.pdf", 5, 3.3)
print(vote_shift)
dev.off()

# Generate Table 4

tbl = do.call(rbind, 
              lapply(list(og, b1, b2), 
                     function(x) {
                       cbind('Experiment' = unique(x$ExpName), 
                             'Group' = unique(x$GroupNumber), 
                             get_vote_shift(x, by = 'GroupNumber'))
                     })
)
names(tbl)[3] = '-1'

# Chi-Sq tests for differences - Table 4
stats = rbind(
  tidy(chisq.test(og$VoteShift, og$GroupNumber))[, 1:2], rep(' ', 2), rep(' ', 2),
  tidy(chisq.test(b1$VoteShift, b1$GroupNumber))[, 1:2], rep(' ', 2), rep(' ', 2),
  tidy(chisq.test(b2$VoteShift, b2$GroupNumber))[, 1:2], rep(' ', 2), rep(' ', 2)
)

# Save Table 4
library(xtable)
latex = xtable(cbind(tbl, stats))
print(latex, sep='\n', file = 'tables/vote-shift.tex')

