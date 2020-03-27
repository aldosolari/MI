#===================
# Modern Inference
# The crisis of Modern Science
# Aldo Solari
#===================

rm(list=ls())

#================================
# Scientific studies
#================================
library(scifigure)
reproduce_figure()
replicate_figure()

#================================
# The likelihood of replicability
#================================

library(ggpubr)
set.seed(123)
n = 200
mu = 2
alpha = 0.05
thr = pnorm(1-alpha, lower.tail = T)
zO = rnorm(n, mean=mu)
zR = rnorm(n, mean=mu)
df = data.frame(original=zO, replication=zR)
df$type = rep("not observed",n)
df$type[zO>thr & zR > thr] <- "replication significant"
df$type[zO>thr & zR <= thr] <- "replication not significant"
library(ggpubr)
ggscatter(df, x="original", y="replication", color="type", palette = c("lightgray", "red", "green")) + geom_vline(xintercept=1.96) + geom_abline(slope = 1, intercept = 0, lty=3) +  geom_hline(yintercept = 0, lty=3) + coord_fixed()
