#===================
# Modern Inference
# FWER control
# Aldo Solari
#===================

rm(list=ls())

# prostate data
zscores <- read.csv("http://web.stanford.edu/~hastie/CASI_files/DATA/prostz.txt", header=F)$V1
pvalues <- 2*pnorm(abs(zscores), lower.tail = FALSE)
hist(pvalues, 20, col="gray", border="white", freq=FALSE, main="", xlab="p")
abline(h=1, col="red")

# 2x2 table
set.seed(123)
alpha = 0.05
m = 100
m0 = 80
effect = 3
setT = sample(1:m, size=m0, replace=F)
stats <- rnorm(m)
stats[-setT] <- stats[-setT] + effect
pvals = pnorm(stats, lower.tail = FALSE)
setR = which(pvals <= alpha)
table( rejected= 1:m %in% setR,
       hypotheses = 1:m %in% setT)

# FWER with i.i.d. null p-values
m = 100
alpha = 0.05
plot(1:m,1-(1-alpha)^(1:m), type="l", lwd=2, xlab="m0", ylab="FWER", ylim=c(0,1))
points(0,0, pch=19, cex=.5)
abline(h=alpha,lty=3)

# PFER with i.i.d. null p-values
m = 100
alpha = 0.05
IC = sapply(1:m, function(i)
  qbinom(c(alpha/2,1-(alpha/2)), size=i, prob=alpha)
)
plot(1:m,(1:m)*alpha, type="l", lwd=2, xlab="m0", ylab="E(V)", ylim=c(0,10))
points(0,0, pch=19, cex=.5)
for (i in 1:m) segments(x0=i,x1=i,y0=IC[1,i],y1=IC[2,i])

