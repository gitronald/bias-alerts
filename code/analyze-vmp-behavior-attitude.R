# VMP by initial attitude strength and search behavior

library(ggplot2)
library(dplyr)

# Load functions
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)

# Load data
load("data/bias_alerts.Rda")


# VMP by Attitude and Search Behavior -------------------------------------

# Create new variables
for(i in 1:5){
  var = paste0("TimePage", i)
  idx = which(study4[, var] > 0)
  study4[idx, 'SearchPageDepth'] = i
}
study4$PastSearches = ntile(study4$SearchesPerWeek, 4)
study4$PreTestAttitudeStrength <- rm_negative(study4$PreTestLikelyVoteBipolar)

# Calculate VMP by alert group and demographic variable
vmp_exp = function(study4, var){
  study4 = split(study4, study4$ExpName)
  vmp_table = rbind(
    cbind("Experiment" = 'No Alert', vmp.data(study4[["Original"]], var, neat = F)),
    cbind("Experiment" = 'Low Alert', vmp.data(study4[["BiasAlert1"]], var, neat = F)),
    cbind("Experiment" = 'High Alert', vmp.data(study4[["BiasAlert2"]], var, neat = F))
  )
  return(vmp_table)
}

vmp_attitude = vmp_exp(study4, 'PreTestAttitudeStrength')
vmp_search_depth = vmp_exp(study4, 'SearchPageDepth')
vmp_past_searches = vmp_exp(study4, 'PastSearches')
vmp_past_searches$Dgroup = factor(vmp_past_searches$Dgroup, 
                                  levels = c('1', '2', '3', '4'), 
                                  labels = c('Low', 'Mod-low', 'Mod-high', 'High'), 
                                  ordered = T)


# Plot vmp tables created using vmp_group_exp
plot_vmp_group = function(vmp_table, xaxis='Title'){
  
  p = ggplot(vmp_table, aes(x = Dgroup, y = VMP, color = Experiment, group = Experiment)) + 
    geom_line(position=position_dodge(0.2)) +
    geom_pointrange(aes(ymin=CImin, ymax=CImax), size=0.6, position=position_dodge(0.2)) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.11,0.9), breaks=seq(0,0.9,0.2)) +
    labs(x = xaxis,
         y = "VMP") +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank(),
          legend.position = c(0.78, 0.78),
          legend.key = element_rect(colour = NA),
          legend.background = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor.y = element_blank()) + 
    scale_colour_manual(values = c("#0072B2", "#CCC814", "#D55E00"))
  return(p)
}

# Create plots for each variable
p_attitude = plot_vmp_group(vmp_attitude, "Initial Attitude Strength")
p_search_depth = plot_vmp_group(vmp_search_depth, "Search Page Depth")
p_past_searches = plot_vmp_group(vmp_past_searches, "Search Activity")

# Combine plots
library(cowplot)
theme_adjustments = theme(legend.position = "none",
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          panel.grid.minor.y = element_blank())
p_search_depth = p_search_depth + theme_adjustments
p_past_searches = p_past_searches + theme_adjustments
p.all = plot_grid(p_attitude, p_search_depth, 
                  align = "h", nrow = 1, rel_widths = c(1.1,1,1))

pdf("plots/vmp-attitude-behavior.pdf", 7, 3.2)
print(p.all)
dev.off()
