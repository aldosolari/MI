#===================
# Modern Inference
# The Law of Selection
# Aldo Solari
#===================

rm(list=ls())

#======================================
# Democrats vs. Republicans: 
# which is better for the U.S. Economy?
#======================================
library(RCurl)
library(tidyr)
library(dplyr)
library(ggpubr)

X <- read.csv("/Users/aldosolari/Dropbox/TEACHING/MI/lawsel/code/X.csv", stringsAsFactors = FALSE)
Y <- read.csv("/Users/aldosolari/Dropbox/TEACHING/MI/lawsel/code/Y.csv", stringsAsFactors = FALSE)
df = merge(X, Y, by.y = "date", by.x = "date", all=T)
df$date <- as.Date(df$date)
df2 = na.omit(df)

# Democrats view
df3 = df2 %>%
  select(date, gdp, gov_dem) %>%
  gather(key = "variable", value = "value", -date)
df3$variable <- factor(df3$variable)
levels(df3$variable) <- c("GDP","Number of Governors (Democrats)")

ggplot(df3, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal() +  xlab("year") + 
  facet_wrap(~ variable, scales = 'free_y', ncol = 1) + theme(legend.position = "none")

ggscatter(df2, x = "gov_dem", y = "gdp",
          add = "reg.line", add.params = list(color = "blue"), 
          xlab="Number of Governors (Democrats)", ylab="GDP")

summary(lm(gdp ~ gov_dem,df2))$coefficients

# Republicans view
df4 = df2 %>%
  select(date, unem, gov_gop) %>%
  gather(key = "variable", value = "value", -date)
df4$variable <- factor(df4$variable, levels=c("unem", "gov_gop"))
levels(df4$variable) <- c("Unemployment","Number of Governors (Republicans)")

ggplot(df4, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  theme_minimal() +  xlab("year") + 
  facet_wrap(~ variable, scales = 'free_y', ncol = 1) + theme(legend.position = "none")

ggscatter(df2, x = "gov_gop", y = "unem",
          add = "reg.line", add.params = list(color = "blue"), 
          xlab="Number of Governors (Republicans)", ylab="Unemployment")

summary(lm(unem ~ gov_gop,df2))$coefficients

#===============================================
# The quite scandal in the statistical community
#===============================================

# variable selection

library(ISLR)
Hitters =na.omit(Hitters)
covs=names(Hitters)[-19]
set.seed(12)
Hitters$Salary = sample(Hitters$Salary)
train = sample(c(T,F), nrow(Hitters),rep=TRUE)
library(leaps)
fit_all = regsubsets(Salary ~ ., Hitters[train,])
fml = paste("Salary ~", paste(covs[summary(fit_all)$which[which.min(summary(fit_all)$cp),-1]], collapse=" + "))
round(
  summary(lm(fml, Hitters[train,]))$coeff
  ,4)
round(
  summary(lm(fml, Hitters[!train,]))$coeff
  ,4)

# Confidence intervals

set.seed(12)
alpha = 0.1
m = 20
theta = rnorm(m,0,sqrt(0.04))
stats = rnorm(m,theta,1)
CIlow = stats - qnorm(alpha, lower.tail = F) 
CIup = stats + qnorm(alpha, lower.tail = F) 
plot(1:m,theta,ylim=c(-3,3), ylab=expression(theta), xlab="i")
abline(h=0)
select = (CIlow*CIup>0)
for (i in 1:m){
  segments(x0=i,x1=i,y0=CIlow[i], y1=CIup[i], col=select[i]+1)
}

#==============
# AIDSvax study
#==============
AIDSvax <- matrix(c(1679-96, 3330-191, 96, 191),
                  nrow = 2,
                  dimnames = list(Group = c("Placebo", "Vaccine"),
                                  Result = c("Not Infected", "Infected")))

fisher.test(AIDSvax)$p.value
White = matrix(c(1508-81, 3003-179, 81, 179),2)
Black = matrix(c(111-9, 203-4, 9, 4),2)
Asian = matrix(c(20-2, 53-4, 2, 4),2)
Other = matrix(c(40-6, 71-6, 6, 6),2)
fisher.test(White)$p.value
fisher.test(Black)$p.value
fisher.test(Asian)$p.value
fisher.test(Other)$p.value





