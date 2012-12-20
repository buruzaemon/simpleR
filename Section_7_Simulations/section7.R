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
title("7.3 - Bernoulli Simulations, p=.25, .05, .02", outer=T)
dev.copy(png, "images/7-1.png")
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
	     xlim=c(-2, 2),
	     ylim=c(0, 4),
	     main=paste("7.6 - exp distibution: n=", as.character(n), sep=""))
	lines(density(results, adjust=2), col="blue")
        Sys.sleep(1)
    }
}

f()
