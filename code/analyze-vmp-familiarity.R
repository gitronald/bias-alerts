# VMP by familiarity

library(ggplot2)
library(dtables)
library(dplyr)

# Load data
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)

# Load functions
load("data/bias_alerts.Rda")

# VMP by Familiarity ------------------------------------------------------

# Create new variables
study4$Familiar = ifelse(study4$FamiliarBoth > 5, 1, 0)
study4$FamiliarCam = ifelse(study4$FamiliarCandidate1 > 5, 1, 0)
study4$FamiliarEd = ifelse(study4$FamiliarCandidate2 > 5, 1, 0)

# Split groups
og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

# Calculate VMP by bias group and demographic variable
vmp_group = function(study4, var){
  study4 = split(study4, study4$GroupNumber)
  vmp_table = rbind(
    cbind("Group" = 'Cameron Bias', vmp.data(study4[["1"]], var, neat = F)),
    cbind("Group" = 'Miliband Bias', vmp.data(study4[["2"]], var, neat = F)),
    cbind("Group" = 'Neutral', vmp.data(study4[["3"]], var, neat = F, group=F))
  )
  return(vmp_table)
}

vmp_cameron = study4 %>%
  group_by(ExpName) %>%
  do(vmp_group(., 'FamiliarCam'))

vmp_miliband = study4 %>%
  group_by(ExpName) %>%
  do(vmp_group(., 'FamiliarEd'))

vmp_familiar = rbind(vmp_cameron, vmp_miliband)
vmp_familiar$Experiment <- factor(vmp_familiar$ExpName,
                                  levels = c("Original", "BiasAlert1", "BiasAlert2"),
                                  labels = c("No Alert", "Low Alert", "High Alert"),
                                  ordered = T)
vmp_familiar$Group <- factor(vmp_familiar$Group,
                             levels = c("Cameron Bias", "Miliband Bias", "Neutral"),
                             labels = c("Cameron Bias", "Miliband Bias", "Neutral"),
                             ordered = T)
vmp_familiar$Familiarity = factor(vmp_familiar$Dgroup, levels=c(0,1), labels=c('Low', 'High'), ordered=T)
vmp_familiar$Demographic = factor(vmp_familiar$Demographic, levels=c('FamiliarCam', 'FamiliarEd'),
                                  labels=c('Cameron', 'Miliband'), ordered=T)

vmp_familiar = vmp_familiar[order(c(vmp_familiar$Experiment, vmp_familiar$Group)), ]
vmp_familiar = vmp_familiar[!is.na(vmp_familiar$ExpName), ]
vmp_familiar = subset(vmp_familiar, Group != "Neutral")

cols <- c("Cameron Bias"="#5B9BD5", "Miliband Bias"="#C00000")

p_vmp_familiar = ggplot(vmp_familiar, aes(y=VMP, x=Familiarity, group=Group, color=Group)) +
  geom_pointrange(aes(ymin=CImin, ymax=CImax)) +
  scale_colour_manual(values = cols) +
  geom_line() + 
  facet_grid(Demographic~Experiment) +
  theme_bw(base_size = 12) +
  scale_y_continuous("VMP",
                     labels = scales::percent_format(),
                     expand = c(0,0),
                     limits = c(-0.05, 0.94)) +
  theme(legend.background = element_blank(),
        legend.position = c(.84, .87),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

pdf("plots/vmp-familiarity.pdf", 5, 3.3)
print(p_vmp_familiar)
dev.off()
