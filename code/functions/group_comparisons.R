
presentation_round = function(number, digits){
  number = as.numeric(number)
  result = format(round(number, digits), digits = digits, nsmall = digits, scientific = F)
  return(result)
}

candidate_tests = function(data1, measure){
  
  mean_se = psych::describeBy(data1[, measure], data1[, 'GroupNumber'], mat = T)
  mean_se = paste0(presentation_round(mean_se$mean, 2), ' (', presentation_round(mean_se$se, 2), ')')
  names(mean_se) = c("Group 1", "Group 2", "Group 3")
  
  kwtest = kruskal.test(data1[, measure], data1[, 'GroupNumber'], p.adjust.method = 'bonferroni')
  kw_extract = c(presentation_round(kwtest$statistic, 3), 
                 kwtest$parameter, 
                 kwtest$p.value)
  names(kw_extract) = c('kw_stat', 'kw_df', 'kw_pval')
  
  g1 = subset(data1, GroupNumber == 1)
  g2 = subset(data1, GroupNumber == 2)
  mwtest = wilcox.test(g1[, measure], g2[, measure], p.adjust.method = 'bonferroni')
  mw_extract = c(presentation_round(mwtest$statistic, 1), 
                 mwtest$p.value)
  names(mw_extract) = c('mw_stat', 'mw_pval')
  
  data_result = c("variable" = measure, mean_se, kw_extract, mw_extract)
  
  return(data_result)
}

p_significance = function(p.value){
  p.value = as.numeric(p.value)
  if (p.value < 0.001) {
    signif_code = "***"
  } else if (p.value < 0.01) {
    signif_code = "**"
  } else if(p.value < 0.05){
    signif_code = "*"
  } else if (p.value < 0.1) {
    signif_code = "."
  } else {
    signif_code = " "
  }
  
  return(signif_code)
}

between_group_testing = function(data1, measures){
  list1 = list()
  for(i in 1:length(measures)){
    result = candidate_tests(data1, measures[i])
    result['kw_padj'] = p.adjust(result['kw_pval'], method = "bonferroni", n = length(measures))
    result['kw_padj'] = presentation_round(result['kw_padj'], 4)
    result['kw_sig'] = p_significance(result['kw_padj'])
    
    result['mw_padj'] = p.adjust(result['mw_pval'], method = "bonferroni", n = length(measures))
    result['mw_padj'] = presentation_round(result['mw_padj'], 4)
    result['mw_sig'] = p_significance(result['mw_padj'])
    
    list1[[i]] = data.frame(t(result))
  }
  
  data2 = cbind('experiment' = deparse(substitute(data1)), do.call(rbind, list1))
  return(data2)
}


se <- function(x) {
  # Standard Error of the Mean
  return(sqrt(var(x)/length(x)))
}

mw.compare <- function (data, groupA, groupB, variable, alt = "two.sided", round = FALSE){
  # Compare differences BETWEEN groups
  # Wilcoxon/Mann-Whitney Rank-Sum Test (unpaired samples)
  # Args:
  #   data: Data object
  #   groupA: Group A = numeric(1:3)
  #   groupB: Group B = numeric(1:3)
  #   variable: Post-Pre difference variable
  #   alt: Alternative hypothesis ("two.sided", "greater", "less")
  #
  experiment <- deparse(substitute(data))
  variable   <- paste(variable)
  dif.groupA <- data[data["GroupNumber"] == groupA, variable]
  dif.groupB <- data[data["GroupNumber"] == groupB, variable]
  deviation  <- dif.groupA - dif.groupB
  mean.A     <- mean(dif.groupA)
  se.A       <- se(dif.groupA)
  mean.B     <- mean(dif.groupB)
  se.B       <- se(dif.groupB)
  mean       <- mean(deviation)
  se         <- se(deviation)
  wilcox     <- wilcox.test(dif.groupA, dif.groupB, alternative = alt, conf.int = TRUE)
  p          <- wilcox$p.value
  statistic  <- wilcox$statistic
  method     <- wilcox$method
  alt        <- wilcox$alternative
  groupA     <- as.factor(groupA)
  groupB     <- as.factor(groupB)
  result     <- data.frame(experiment, groupA, groupB, variable, mean.A, se.A, mean.B, se.B, mean, se, statistic, p, method, alt)
  rownames(result) <- NULL
  if (round){
    result[1, 1:2] <- round(result[1, 1:2], digits = 2)
  }
  return (result)   
}

mw.plot <- function(data, title) {
  ggplot(data[data["variable"] != "DifLikelyVote", ], 
         aes(x=variable, y=mean)) + 
    ggtitle(title) +
    ylab("Attitude Shift") +
    geom_pointrange(aes(ymin=mean-se, ymax=mean+se, colour=groupA, shape=groupA), size=1.2) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(data$variable)[-1]),
                     labels = c("Impression Miliband", "Like Miliband", "Trust Miliband",
                                "Impression Cameron", "Like Cameron", "Trust Cameron")) +
    scale_y_continuous(limit = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, 0.5), expand=c(0,0)) + 
    scale_color_manual(name="Group", values = c("#E82D30", "#3292CC"), labels = c("Cameron Bias", "Miliband Bias")) +
    scale_shape_discrete(name = "Group", labels = c("Cameron Bias", "Miliband Bias")) +
    theme(title = element_text(size = 15, face = "bold", vjust = 1.5),
          axis.title.x = element_text(size = 15, face = "plain", vjust = -.5),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 12, colour = "#2B2B2B"),
          panel.background = element_rect(colour = "#E5E5E5"),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "#FFFFFF"),
          panel.grid.minor = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
}