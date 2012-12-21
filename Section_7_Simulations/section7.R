##
# code for something like Figure 31
# n = 10
# p = .25
# hist(rbinom(100,n,p), breaks=seq(0,6,1), col=gray(.9), prob=T)
# lines(seq(0,6,1), dbinom(seq(0,6,1), n, p), col='green')
# dev.copy(png, "images/7-fig-31.png")
# dev.off()
library(UsingR)

## Q 7.1
set.seed(7.1)
n = 100
p = 0.5
S1 <- rbinom(1000,n,p)
S2 <- rbinom(1000,n,p)
X1 <- (S1 - n*p)/sqrt(n*p*(1-p))
X2 <- (S2 - n*p)/sqrt(n*p*(1-p))
par(mfrow=c(1,2), oma=c(0,0,2,0))
hist(X1, col=gray(.9), prob=T, main="S1")
lines(density(X1, adjust=2), col="red")
hist(X2, col=gray(.9), prob=T, main="S2")
lines(density(X2, adjust=2), col="red")
title("7.1 - Binomial Simulation Comparison", outer=T)
dev.copy(png, "images/7-1.png")
dev.off()

#  ... S1 and S2 will yield different sets of numbers,
#      so their respective density graphs will be different.
#      range and shape should be similar, however...


## Q 7.2
set.seed(7.2)
n = 10
m1 = 10
s1 = 10
m2 = 100
s2 = 100
S1 <- rnorm(10, m1, s1)
S2 <- rnorm(10, m2, s2)
par(mfrow=c(1,2), oma=c(0,0,2,0))
qqnorm(S1, main="S1")
qqline(S1, col="red")
qqnorm(S2, main="S2")
qqline(S2, col="red")
title("7.2 - Normal Simulation Comparison", outer=T)
dev.copy(png, "images/7-2.png")
dev.off()


## Q 7.3
set.seed(7.3)
n = 100
f <- function(n=100, p=.5)
{
    S <- rbinom(1,n,p)
    (S - n*p)/sqrt(n*p*(1-p))
}
S1 <- simple.sim(1000,f,n,.25)
S2 <- simple.sim(1000,f,n,.05)
S3 <- simple.sim(1000,f,n,.01)
par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(S1, col=gray(.9), prob=T, main="S1: p=0.25")
lines(density(S1, adjust=2), col="blue")
hist(S2, col=gray(.9), prob=T, main="S2: p=0.05")
lines(density(S2, adjust=2), col="blue")
hist(S3, col=gray(.9), prob=T, main="S3: p=0.01")
lines(density(S3, adjust=2), col="blue")
title("7.3 - Bernoulli Simulations, p=.25, .05, .01", outer=T)
dev.copy(png, "images/7-3.png")
dev.off()

#  ... yes, you can see that the distribution is 
#      approximately normal for S1 and even S2,
#      and then it goes south for S3...


## Q 7.4
set.seed(7.4)
m = 0
s = 1
x <- rnorm(100,m,s)
f <- function(X,k=1,s=1)
{
    sum(-k*s < X & X < k*s)/length(X)
}

f(x,1,s)
# [1] 0.75

f(x,2,s)
# [1] 0.95

f(x,3,s)
# [1] 1


## Q 7.5
set.seed(7.5)

#  ... this is a continuous uniform distribution
#      c.f. http://en.wikipedia.org/wiki/Uniform_distribution_(continuous))
f <- function(n, a=0, b=1)
{
    m <- (b+a)/2
    s <- (b-a)/sqrt(12)
    (mean(runif(n,a,b))-m)/(s/sqrt(n))
}

X1 <- simple.sim(100,f,1)
X5 <- simple.sim(100,f,5) 
X10 <- simple.sim(100,f,10) 
X25 <- simple.sim(100,f,25)

par(mfrow=c(2,2), oma=c(0,0,2,0))

hist(X1, col=gray(.9), prob=T, main="n=1")
lines(density(X1, adjust=2), col="blue")

hist(X5, col=gray(.9), prob=T, main="n=5")
lines(density(X5, adjust=2), col="blue")

hist(X10, col=gray(.9), prob=T, main="n=10")
lines(density(X10, adjust=2), col="blue")

hist(X25, col=gray(.9), prob=T, main="n=25")
lines(density(X25, adjust=2), col="blue")

title("7.5 - Uniform Dist. Simulations: n=1, 5, 10, 25", outer=T)
dev.copy(png, "images/7-5.png")
dev.off()


## Q 7.6
set.seed(7.6)

f <- function()
{
    for (n in 1:50) {
        results = c()
        mu = 10;sigma = mu
        for (i in 1:200) {
            X = rexp(200,1/mu)
            results[i] = (mean(X)-mu)/(sigma/sqrt(n))
        }
        hist(results, 
	     prob=T, 
	     col=gray(.9),
	     xlim=c(-2, 2),
	     ylim=c(0, 4),
	     main=paste("7.6 - exp distribution: n=", as.character(n), sep=""))
	lines(density(results, adjust=2), col="blue")
        Sys.sleep(1)
    }
}

f()


## Q 7.7
set.seed(7.7)

par(mfrow=c(2,2), oma=c(0,0,2,0))

R1 <- rt(100,4)
R2 <- rt(100,50)
R3 <- rchisq(100,4)
R4 <- rchisq(100,50)

qqnorm(R1, main="rt(100, 4)")
qqline(R1, col="red")

qqnorm(R2, main="rt(100, 50)")
qqline(R2, col="red")

qqnorm(R3, main="rchisq(100, 4)")
qqline(R3, col="red")

qqnorm(R4, main="rchisq(100, 50)")
qqline(R4, col="red")

title("7.7 - Normal plots: Student's T and Chi-Squared", outer=T)
dev.copy(png, "images/7-7.png")
dev.off()

#  ... only the 1st, 2nd, and 4th distributions look like
#      they could be normal


## Q 7.8
set.seed(7.8)
data(faithful)

summary(faithful)
#   eruptions        waiting    
# Min.   :1.600   Min.   :43.0  
# 1st Qu.:2.163   1st Qu.:58.0  
# Median :4.000   Median :76.0  
# Mean   :3.488   Mean   :70.9  
# 3rd Qu.:4.454   3rd Qu.:82.0  
# Max.   :5.100   Max.   :96.0

bootstrap <- function(data, n=length(data))
{
    boot.sample <- sample(data, n, replace=T)
    median(boot.sample)
}

S <- simple.sim(100, bootstrap, faithful$eruptions)
summary(S)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  3.350   3.934   4.000   3.971   4.021   4.167

par(mfrow=c(1,2), oma=c(0,0,2,0))

qqnorm(S, main="Normal Plot of Sample S")
qqline(S, col="red")
#  ... no, this plot doesn't really look like it could be normal

hist(S, prob=T, col=gray(.9), main="Prob. Distribution of Sample S")
lines(density(S), col="blue")

title("7.8 - Bootstrap Technique: Sample median from faithful$eruptions", outer=T)
dev.copy(png, "images/7-7.png")
dev.off()


## Q 7.9
set.seed(7.9)

#  ... let's first try a Normal distribution...
res.median <- c()
res.mean   <- c()

for (i in 1:200) 
{
    X = rnorm(200,0,1)
    res.median[i] <- median(X)
    res.mean[i]   <- mean(X)
}

par(mfrow=c(2,2), oma=c(0,0,2,0))

#  boxplot: rnorm
boxplot(res.mean, res.median, 
    main="rnorm(200, 0, 1)", 
    names=c("mean", "median"),
    col=c("blue", "red"))

#  histogram for comparison: rnorm
x <- rnorm(200,0,1)
hist(x, col=gray(.9), main="rnorm(200,0,1)")
abline(v=mean(x), col="blue", lwd=2, lty=1)
abline(v=median(x), col="red", lwd=2, lty=2)
legend(locator(1), 
       c("mean", "median"), 
       col=c("blue", "red"), 
       cex=.7, 
       lty=1:2)

#  ... mean: as this considers ALL values in distribution,
#            it is particularly susceptible to the influence
#            of outliers
#    median: prefer median over mean in case of non-normal,
#            skewed distributions
#      NOTE: see how and where the outliers are appearing???

#  ... now let's do a long-tailed distribution!
res.median <- c()
res.mean   <- c()

for (i in 1:200) 
{
    X = rt(200,2)
    res.median[i] <- median(X)
    res.mean[i]   <- mean(X)
}

#  boxplot: rt
boxplot(res.mean, res.median, 
    main="rt(200, 2)", 
    names=c("mean", "median"),
    col=c("blue", "red"))

#  histogram for comparison: rt
x <- rt(200,2)
hist(x, col=gray(.9), main="rt(200,2)")
abline(v=mean(x), col="blue", lwd=2, lty=1)
abline(v=median(x), col="red", lwd=2, lty=2)
legend(locator(1), 
       c("mean", "median"), 
       col=c("blue", "red"), 
       cex=.7, 
       lty=1:2)

title("7.9 - Mean and Median: Normal & Long-Tailed Distributions", outer=T)
dev.copy(png, "images/7-9.png")
dev.off()


## 7.10
set.seed(7.10)

par(mfrow=c(1,3), oma=c(0,0,2,0))

#  first, compare mean/median in a normal distribution
mean.normal   <- function(n=100) mean(rnorm(n,0,1))
median.normal <- function(n=100) median(rnorm(n,0,1))

S.mean.norm    <- simple.sim(100, mean.normal)
S.median.norm  <- simple.sim(100, median.normal)
var(S.mean.norm) / var(S.median.norm)
# [1] 0.4991862

boxplot(S.mean.norm, S.median.norm,
        main="normal",
	names=c("mean", "median"),
	col=c("blue", "red"))

#  next: exponential distribution
mean.exp   <- function(n=100) mean(rexp(n,1/10))
median.exp <- function(n=100) median(rexp(n,1/10))

S.mean.exp    <- simple.sim(100, mean.exp)
S.median.exp  <- simple.sim(100, median.exp)
var(S.mean.exp) / var(S.median.exp)
# [1] 0.703916 

boxplot(S.mean.exp, S.median.exp,
        main="exponential",
	names=c("mean", "median"),
	col=c("blue", "red"))

#  last: t-distribution
mean.t   <- function(n=100) mean(rt(n,2))
median.t <- function(n=100) median(rt(n,2))

S.mean.t    <- simple.sim(100, mean.t)
S.median.t  <- simple.sim(100, median.t)
var(S.mean.t) / var(S.median.t)
# [1] 5.305217

boxplot(S.mean.t, S.median.t,
        main="Student's t",
	names=c("mean", "median"),
	col=c("blue", "red"))

title("7.10 - Mean and Median: Normal, Exp, & Student's t Distributions", outer=T)
dev.copy(png, "images/7-10.png")
dev.off()

