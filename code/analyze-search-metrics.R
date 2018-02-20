# Search metrics analysis

library(ggplot2)
library(dtables)

# Load functions from /R
R.files <- list.files("code/functions", full.names = T)
invisible(lapply(R.files, source)); rm(R.files) 

# Load data
load("data/bias_alerts.Rda")

og <- subset(study4, ExpName == "Original")
b1 <- subset(study4, ExpName == "BiasAlert1")
b2 <- subset(study4, ExpName == "BiasAlert2")

# By Seconds --------------------------------------------------------------

# Create variable names for timer and clicks
timer.names <- data.frame("rank" = 1:30, "type" = "timer")
timer.vars   <- paste0("Timer", 1:30)
click.names <- data.frame("rank" = 1:30, "type" = "click")
click.vars   <- paste0("Clicks", 1:30)
page.names <- data.frame("rank" = 1:5, "type" = "serp")
page.vars   <- paste0("TimePage", 1:5)

# Get descriptive statistics by timers, clicks and SERPs
shape_search = function(data1){
  dtbl.ts <- cbind(timer.names, dtable(data1, timer.vars, neat = F)$Statistics)
  dtbl.tc <- cbind(click.names, dtable(data1, click.vars, neat = F)$Statistics)
  dtbl.tp <- cbind(page.names, dtable(data1, page.vars, neat = F)$Statistics)
  dtbl = rbind(dtbl.ts, dtbl.tc, dtbl.tp)
  return(dtbl)
}

og.biasg = subset(og, GroupNumber < 3)
b1.biasg = subset(b1, GroupNumber < 3)
b2.biasg = subset(b2, GroupNumber < 3)

search.og = shape_search(og.biasg)
search.b1 = shape_search(b1.biasg)
search.b2 = shape_search(b2.biasg)

search = rbind(cbind(Experiment = rep("No Alert", nrow(search.og)), search.og),
               cbind(Experiment = rep("Low Alert", nrow(search.b1)), search.b1),
               cbind(Experiment = rep("High Alert", nrow(search.b2)), search.b2))

# Plot SERP timers --------------------------------------------------------

dat = subset(search, type == "serp")
serp = ggplot(dat, 
           aes(x = as.factor(rank), y = mean, 
               group = Experiment, colour = Experiment, shape = Experiment)) + 
  geom_line(position=position_dodge(0.12)) +
  geom_pointrange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), position=position_dodge(0.12)) +
  labs(x = "Page",
       y = "Average Time (s)") +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw(base_size = 24)

# Color and theme adjustments
serp = serp + scale_colour_manual(values = c("#0072B2", "#CCC814", "#D55E00"))
serp = serp + theme(legend.title = element_blank(),
              legend.position=c(0.8,0.9),
              legend.key = element_rect(colour = NA),
              legend.background = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5))
serp = serp + theme(axis.text.x = element_text(margin = margin(b = 7, unit = "pt")),
                    axis.text.y = element_text(margin = margin(l = 10, unit = "pt")))

# Save plot
pdf("plots/ByExperiment-SERP.pdf", 7, 5.6)
print(serp)
dev.off()

# Plot Webpage clicks -----------------------------------------------------

dat = subset(search, type == "click")
click = ggplot(dat, 
           aes(x = as.factor(rank), 
               y = mean, 
               group = Experiment, colour = Experiment, shape = Experiment)) + 
  geom_line(position=position_dodge(0.2)) +
  geom_pointrange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), position=position_dodge(0.2)) +
  scale_x_discrete(breaks=c(1,seq(6,30,by=6))) + 
  labs(x = "Search Ranking",
       y = "Average Clicks") +
  theme_bw(base_size = 24)  
  
## Add SERP Lines
click = click + geom_vline(xintercept = c(6.5, 12.5, 18.5, 24.5), colour = "#919191")

# Color and theme adjustments
click = click + scale_colour_manual(values = c("#0072B2", "#CCC814", "#D55E00"))
click = click + theme(legend.title = element_blank(),
                      legend.position='none',
                      legend.key = element_rect(colour = NA),
                      legend.background = element_blank(),
                      panel.grid.major.x = element_blank(),
                      plot.title = element_text(hjust = 0.5),
                      axis.text.x = element_text(margin = margin(b = 7, unit = "pt")),
                      axis.text.y = element_text(margin = margin(l = 10, unit = "pt")))

# Save plot
pdf("plots/ByExperiment-Clicks.pdf", 7, 5.6)
print(click)
dev.off()

# Plot Webpage timers -----------------------------------------------------

dat = subset(search, type == "timer")
timer = ggplot(dat, 
                 aes(x = as.factor(rank), 
                     y = mean, 
                     group = Experiment, colour = Experiment, shape = Experiment)) + 
  geom_line(position=position_dodge(0.2)) +
  geom_pointrange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), position=position_dodge(0.2)) +
  labs(x = "Search Ranking",
       y = "Average Time (s)") +
  scale_x_discrete(breaks=c(1,seq(6,30,by=6))) +
  theme_bw(base_size = 24)

## Add SERP Lines
timer = timer + geom_vline(xintercept = c(6.5, 12.5, 18.5, 24.5), colour = "#919191")

timer = timer + scale_colour_manual(values = c("#0072B2", "#CCC814", "#D55E00"))
timer = timer + theme(axis.text.x = element_text(margin = margin(b = 7, unit = "pt")),
              axis.text.y = element_text(margin = margin(l = 10, unit = "pt")))
timer = timer + theme(legend.title = element_blank(),
              legend.position='none',
              legend.key = element_rect(colour = NA),
              legend.background = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5))

pdf("plots/ByExperiment-Timer.pdf", 7, 5.6)
print(timer)
dev.off()


# Significance tests ------------------------------------------------------

library(broom)

# Timer

dat1 = subset(search.og, type == "timer")
dat2 = subset(search.b1, type == "timer")
dat3 = subset(search.b2, type == "timer")

ks_tests_timers = rbind(
  tidy(ks.test(dat1$mean, dat2$mean, alternative = "greater")),
  tidy(ks.test(dat2$mean, dat3$mean, alternative = "greater")),
  tidy(ks.test(dat1$mean, dat3$mean, alternative = "greater"))
)

cor_tests_timers = rbind(
  tidy(cor.test(dat1$rank, dat1$mean, method = "spearman")),
  tidy(cor.test(dat2$rank, dat2$mean, method = "spearman")),
  tidy(cor.test(dat3$rank, dat3$mean, method = "spearman"))
)

print(ks_tests_timers)
print(cor_tests_timers)

# Clicks

dat1 = subset(search.og, type == "click")
dat2 = subset(search.b1, type == "click")
dat3 = subset(search.b2, type == "click")

ks_tests_clicks = rbind(
  tidy(ks.test(dat1$mean, dat2$mean, alternative = "greater")),
  tidy(ks.test(dat2$mean, dat3$mean, alternative = "greater")),
  tidy(ks.test(dat1$mean, dat3$mean, alternative = "greater"))
)

cor_tests_clicks = rbind(
  tidy(cor.test(dat1$rank, dat1$mean, method = "spearman")),
  tidy(cor.test(dat2$rank, dat2$mean, method = "spearman")),
  tidy(cor.test(dat3$rank, dat3$mean, method = "spearman"))
)

print(ks_tests_clicks)
print(cor_tests_clicks)


# SERP

dat1 = subset(search.og, type == "serp")
dat2 = subset(search.b1, type == "serp")
dat3 = subset(search.b2, type == "serp")

ks_tests_serp = rbind(
  tidy(ks.test(dat1$mean, dat2$mean, alternative = "greater")),
  tidy(ks.test(dat2$mean, dat3$mean, alternative = "greater")),
  tidy(ks.test(dat1$mean, dat3$mean, alternative = "greater"))
)

print(ks_tests_serp)

