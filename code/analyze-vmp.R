# Vote shift metrics

library(ggplot2)

R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)
load("data/bias_alerts.Rda")

og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

# VMP by Experiment -------------------------------------------------------

vmp.data(study4, 'ExpName')

# Significance of differences
prop.test(x=c(153, 88), n=c(392, 398)) # No Alert and Low Alert
prop.test(x=c(153, 52), n=c(392, 376)) # No Alert and High Alert
prop.test(x=c(88, 52), n=c(398, 376)) # Low Alert and High Alert

# Get vmp by exp and bias group
vmp.og = vmp.data(og, "GroupNumber", group = F, neat = F)
vmp.b1 = vmp.data(b1, "GroupNumber", group = F, neat = F)
vmp.b2 = vmp.data(b2, "GroupNumber", group = F, neat = F)

vmp_group = do.call(rbind.data.frame, list(vmp.og, vmp.b1, vmp.b2))

vmp_group$Experiment <- factor(vmp_group$Study,
                               levels = c("og", "b1", "b2"),
                               labels = c("No Alert", "Low Alert", "High Alert"),
                               ordered = T)

vmp_group$Group <- factor(vmp_group$Dgroup,
                          levels = c(1, 2, 3),
                          labels = c("Cameron Bias", "Miliband Bias", "Neutral"),
                          ordered = T)

p = ggplot(vmp_group, aes(x = Group, y = VMP, fill = Group)) + 
  geom_bar(stat = "identity",  position=position_dodge()) +
  geom_errorbar(ymin = vmp_group$CImin, ymax = vmp_group$CImax, width = 0.28) +
  facet_grid(.~Experiment) +
  scale_y_continuous("VMP",
                     labels = scales::percent_format(),
                     expand = c(0,0),
                     lim = c(-0.02, 0.61)) +
  theme_bw(base_size = 10) +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12, margin = margin(r = 1, unit = "pt")),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, margin = margin(l = 10, unit = "pt")),
        axis.ticks.x = element_blank()) +
  theme(legend.title = element_blank(),
        legend.position=c(0.83,0.74),
        legend.key.size = unit(1, 'line'),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_manual(values = c("#5B9BD5", "#C00000", '#458B00'))

pdf("plots/vmp-exp-group.pdf", 6, 2)
print(p)
dev.off()
