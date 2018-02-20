# Bias Awareness

# Load functions
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files)

# Load data
load("data/bias_alerts.Rda")

# Bias Detection ----------------------------------------------------------

# Keywords for bias detection, see Appendix of paper
detector.text <- as.character("biased | bias | 
                              leaning towards | leaning toward | leaning against |
                              slanted | slanted toward | 
                              skewed | skewed toward | results favor | results favored | one sided | 
                              favorable toward | favorable towards | 
                              favorable for | favorable against | favorable results | 
                              favored towards | favored toward | favored for | 
                              favored against | favored results | 
                              favour toward | favourable towards | favourable toward | 
                              favourable for | favourable against | favourable results | 
                              favoured towards | favoured toward | favoured for | 
                              favoured against | favoured results | favour toward | 
                              results favour | results favoured | 
                              favor Cameron | favor Miliband | 
                              favour Cameron | favour Miliband | 
                              pro Cameron | pro Miliband | 
                              pro-Cameron | pro-Miliband | 
                              Cameron leaning  | Miliband leaning | 
                              negative toward | negative for | negative against | 
                              postive toward | postive for | postive against | 
                              all postive | all negative | 
                              mainly positive | mainly negative | 
                              mostly positive | mostly negative | 
                              more negativity | 
                              nothing positive | nothing negative | 
                              more results for | less results for | 
                              most of the articles were negative | 
                              most of the articles were positive")

# Remove line breaks from detector.text
detector.text <- gsub("\n", "", detector.text)

study4$Detector <- grepl(detector.text, study4$BotheredText, ignore.case=TRUE)
table(study4$Detector, study4$ExpName)

# Differences in Proportions ----------------------------------------------

og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

og.biasg = subset(og, GroupNumber < 3)
b1.biasg = subset(b1, GroupNumber < 3)
b2.biasg = subset(b2, GroupNumber < 3)

prop_compare = function(data1, data2, var1){
  res = prop.test(c(sum(data1[, var1]), sum(data2[, var1])),
                  c(length(data1[, var1]), length(data2[, var1])))
  return(res)
}

l = list(
  prop_compare(og.biasg, b1.biasg, "Detector"),
  prop_compare(b1.biasg, b2.biasg, "Detector"),
  prop_compare(og.biasg, b2.biasg, "Detector")
)

awareness_sig = do.call(rbind, lapply(l, broom::tidy))
awareness_sig$data.name = c("ogb1", "b1b2", "ogb2")

print(awareness_sig)

# VMP by Detector ---------------------------------------------------------

og.biasg = subset(og, GroupNumber < 3)
b1.biasg = subset(b1, GroupNumber < 3)
b2.biasg = subset(b2, GroupNumber < 3)

vmp.detector = rbind(
  vmp.data(og.biasg, "Detector"),
  vmp.data(b1.biasg, "Detector"),
  vmp.data(b2.biasg, "Detector")
)

print(vmp.detector)

# Compare significance of differences: Aware vs. Not aware
prop.test(x=c(vmp.detector[1, 'Nshift'], vmp.detector[2, 'Nshift']), 
          n=c(vmp.detector[1, 'Npre'], vmp.detector[2, 'Npre'])) # No alert

prop.test(x=c(vmp.detector[3, 'Nshift'], vmp.detector[4, 'Nshift']), 
          n=c(vmp.detector[3, 'Npre'], vmp.detector[4, 'Npre'])) # Low alert

prop.test(x=c(vmp.detector[5, 'Nshift'], vmp.detector[6, 'Nshift']), 
          n=c(vmp.detector[5, 'Npre'], vmp.detector[6, 'Npre'])) # High alert


# Impact of awareness of search -------------------------------------------

# Shape search data by ranking and bias awareness
shape_search_detectors = function(data1){
  biasg_detect = subset(data1, GroupNumber < 3 & Detector==T)
  biasg_nodetect = subset(data1, GroupNumber < 3 & Detector==F)
  search_detect = shape_search(biasg_detect)
  search_nodetect = shape_search(biasg_nodetect)
  search = rbind(
    cbind(Detector=rep('Aware',nrow(search_nodetect)), search_detect ),
    cbind(Detector=rep('Unaware',nrow(search_nodetect)), search_nodetect))
  return(search)
}

search.og = shape_search_detectors(og)
search.b1 = shape_search_detectors(b1)
search.b2 = shape_search_detectors(b2)

search = rbind(cbind(Experiment = rep("No Alert", nrow(search.og)), search.og),
               cbind(Experiment = rep("Low Alert", nrow(search.b1)), search.b1),
               cbind(Experiment = rep("High Alert", nrow(search.b2)), search.b2))

library(broom)
compare_search_data = function(data1, metric, alt="less"){
  d1 = subset(data1, type == metric & Detector == 'Aware')
  d2 = subset(data1, type == metric & Detector == 'Unaware')
  row = cbind("type"=metric, tidy(ks.test(d1$mean, d2$mean, alternative = alt)))
  return(row)
}

compare_search_data_all = function(data){
  types = c('timer', 'click', 'serp')
  l = lapply(types, function(x) compare_search_data(data, x))
  df = do.call(rbind, l)
  return(df)
}

ks_og_detectors = compare_search_data_all(search.og)
ks_b1_detectors = compare_search_data_all(search.b1)
ks_b2_detectors = compare_search_data_all(search.b2)

print(ks_og_detectors)
print(ks_b1_detectors)
print(ks_b2_detectors)