# VMP by demographics

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

# VMP by demographics -----------------------------------------------------

dvars = c("Gender", "Education", "Ethnicity", "IncomeComb", 
          "Marital", "Employed", "PoliticalView", "Religion",
          "CountryComb", "EverSearched")

og_vmp_demographics = do.call(rbind, lapply(dvars, function(x) vmp.data(og, x, neat = F)))
b1_vmp_demographics = do.call(rbind, lapply(dvars, function(x) vmp.data(b1, x, neat = F)))
b2_vmp_demographics = do.call(rbind, lapply(dvars, function(x) vmp.data(b2, x, neat = F)))

demographics = demographics = dtable(study4, dvars, neat = F)
demographics = demographics$Frequencies[, 2:5]
names(demographics) = c('demographics', 'group', 'n', 'prop')

# Edit variable and group names
demographics$demographic = gsub('Comb', '', demographics$demographic)
demographics$demographic = gsub('PoliticalView', 'Political View', demographics$demographic)
demographics$demographic = gsub('EverSearched', 'Political\nSearch', demographics$demographic)
demographics$demographic = gsub('PastSearches', 'Search\nActivity', demographics$demographic)

demographics$group = gsub('Never married', 'Never Married', demographics$group)
demographics$group = gsub('1', 'Low', demographics$group)
demographics$group = gsub('2', 'Moderate-low', demographics$group)
demographics$group = gsub('3', 'Moderate-high', demographics$group)
demographics$group = gsub('4', 'High', demographics$group)

# Formatted tables with Bonferroni corrections
og_vmp_demographics$CI = paste0('(', gsub('%', '', paste(og_vmp_demographics$CImin, '-', og_vmp_demographics$CImax)), ')')
og_vmp_demographics$sig = unlist(lapply(og_vmp_demographics$P, p_signif, ns_exact = F))
og_vmps = cbind(demographics[, c('demographic','group')], og_vmp_demographics[, c('VMP', 'CImax', 'CImin', 'Chi', 'P', 'sig', 'N')])
og_vmps$p_corr = p.adjust(og_vmps$P, 'bonferroni', n = length(og_vmps$P))
og_vmps$sig2 = unlist(lapply(og_vmps$p_corr, p_signif, ns_exact = F))

b1_vmp_demographics$CI = paste0('(', gsub('%', '', paste(b1_vmp_demographics$CImin, '-', b1_vmp_demographics$CImax)), ')')
b1_vmp_demographics$sig = unlist(lapply(b1_vmp_demographics$P, p_signif, ns_exact = F))
b1_vmps = cbind(demographics[, c('demographic','group')], b1_vmp_demographics[, c('VMP', 'CImax', 'CImin', 'Chi', 'P', 'sig', 'N')])
b1_vmps$p_corr = p.adjust(b1_vmps$P, 'bonferroni', n = length(b1_vmps$P))
b1_vmps$sig2 = unlist(lapply(b1_vmps$p_corr, p_signif, ns_exact = F))

b2_vmp_demographics$CI = paste0('(', gsub('%', '', paste(b2_vmp_demographics$CImin, '-', b2_vmp_demographics$CImax)), ')')
b2_vmp_demographics$sig = unlist(lapply(b2_vmp_demographics$P, p_signif, ns_exact = F))
b2_vmps = cbind(demographics[, c('demographic','group')], b2_vmp_demographics[, c('VMP', 'CImax', 'CImin', 'Chi', 'P', 'sig', 'N')])
b2_vmps$p_corr = p.adjust(b2_vmps$P, 'bonferroni', n = length(b2_vmps$P))
b2_vmps$sig2 = unlist(lapply(b2_vmps$p_corr, p_signif, ns_exact = F))

# Rebind experment data
all_vmps = rbind(cbind('study' = 'No Alert', og_vmps), 
                 cbind('study' = 'Low Alert', b1_vmps), 
                 cbind('study' = 'High Alert', b2_vmps))

# Subset data - see fig 6 caption
all_vmps2 = subset(all_vmps, N > 60)
all_vmps2 = subset(all_vmps2, group != 'Divorced')

all_vmps2$p_corr2 = p.adjust(all_vmps2$P, 'bonferroni', n = length(all_vmps2$P))
all_vmps2$sig3 = unlist(lapply(all_vmps2$p_corr2, p_signif, ns_exact = F))

# Significant demographic VMPs by experiment
dft(subset(all_vmps2, study == 'No Alert')$sig3)
dft(subset(all_vmps2, study == 'Low Alert')$sig3)
dft(subset(all_vmps2, study == 'High Alert')$sig3)

# Generate multiplot - Figure 6 in Appendix
plot_list = list()
count = 0
unique_demographics = c('Country', 'Political View', 'Political\nSearch', 'Employed', 'Marital')#, 'Search\nActivity')
for(ud in unique_demographics){
  count = count + 1
  print(count)
  plot_list[[ud]] = ggplot(subset(all_vmps2, demographic == ud), 
                           aes(x = group, y = VMP, group = study, color = study)) + 
    geom_pointrange(aes(ymin = CImin, ymax = CImax), position=position_dodge(0.3)) + 
    scale_colour_manual(values = c("#0072B2", "#CCC814", "#D55E00")) +
    scale_y_continuous(expand = c(0,0), limits = c(-0.025, 0.81), labels=scales::percent_format()) + 
    facet_grid(demographic ~ .) + 
    geom_hline(yintercept = 0, color = "#777777") + 
    coord_flip() + 
    theme_bw(base_size = 10) + 
    theme(axis.text.y = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          axis.title.y=element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
  # Set legend in first plot
  if (count == 1){ 
    plot_list[[ud]] = plot_list[[ud]] + 
      theme(legend.title = element_blank(),
            legend.position=c(0.8,0.6),
            legend.key = element_rect(colour = NA, size = 0.5),
            legend.text = element_text(size = 9),
            legend.background = element_blank())
  } else {
    plot_list[[ud]] = plot_list[[ud]] + theme(legend.position = 'none')
  }
  if (count < length(unique_demographics)){
    plot_list[[ud]] = plot_list[[ud]] + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
}

# Correction for x-axis orders
plot_list[[2]] = plot_list[[2]] + scale_x_discrete(limits = c('Liberal', 'Moderate', 'Conservative'))

library(cowplot)
vmp_demographics = plot_grid(plotlist = plot_list, align = "v", ncol = 1, rel_heights = c(1.4, 1.4, 1.1, 1.1, 1.1))

pdf("plots/vmp-by-demographics.pdf", 4.2, 6)
print(vmp_demographics)
dev.off()

