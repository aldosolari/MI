#===================
# Modern Inference
# Lecture 1
# Aldo Solari
#===================

rm(list=ls())

#===== Section 1
library(SMPracticals)
data(darwin)
darwin_pair = data.frame(Pot = darwin[darwin$type=="Cross",1],
                         Cross = darwin[darwin$type=="Cross",4],
                         Self = darwin[darwin$type=="Self",4])
darwin_pair
aggregate(height~type, data=darwin, FUN="mean")

#===== Section 1.1
set.seed(123)
n <- 100
coin <- c("HEAD","HEAD","TAIL") # p = 2/3
x <- sample(coin, size=n, replace = TRUE)
table(x)
#pdf("coin.pdf")
plot(0:n, dbinom(0:n,size=n,prob=1/2),type="h", xlab="# heads", ylab="Probability")
points(sum(x=="HEAD"),0,col=2)
#dev.off()
pval <- 2*min(
  pbinom(sum(x=="HEAD"), size = length(x), lower.tail = F, prob=0.5),
  pbinom(sum(x=="HEAD"), size = length(x), lower.tail = T, prob=0.5))
pval

#===== Section 1.2
library(ggpubr)
p <- ggboxplot(darwin, x = "type", y = "height",
               color = "type", add = "jitter")
p
#pdf("boxplot.pdf");print(p);dev.off()

t.test(height ~ type, data=darwin, var.equal=TRUE)

#===== Section 1.3
p <- ggline(darwin, x = "type", y = "height", group="pair")
p
#pdf("pairs.pdf");print(p);dev.off()

differences = apply(darwin_pair[,3:2],1,diff)
t.test(differences)


#===== Section 3
binom.test(x=13, n=15, p=0.5, alternative="two.sided")

#===== Section 4

#pdf("ecdf.pdf")
plot(ecdf(differences), verticals=T, pch=".", main="", ylab="Distribution function", xlab="y")
curve(pnorm(x, mean=mean(differences), sd=sd(differences)), add=T)
#dev.off()

t_obs = ks.test(differences, "pnorm", mean(differences), sd(differences))
t_obs

set.seed(123)
t_b = sapply(1:1000, function(b)
  ks.test(rnorm(15,mean(differences), sd(differences)),
          "pnorm", mean(differences), sd(differences))$statistic
)
(1+sum(t_b >= t_obs$statistic))/(1000+1)

#pdf("MCnull.pdf")
hist(t_b, xlab="T", 50, main="")
#dev.off()