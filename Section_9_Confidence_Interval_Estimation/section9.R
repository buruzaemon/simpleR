library(UsingR)

## Q 9.1
#  ... assuming that sigma is known
#      and the X_i are normally distributed...
simple.z.test <- function(X, sigma, conf.level=0.95)
{
    n = length(X)
    xbar = mean(X)
    alpha = 1 - conf.level
    zstar = qnorm(1 - alpha/2)
    SE = sigma/sqrt(n)
    xbar + c(-zstar*SE, zstar*SE)
}

set.seed(9.1)
mu = 10
sd = 5

x <- rnorm(15, mu, sd)

simple.eda(x)
dev.copy(png, "images/9-1.png")
dev.off()

#  ... perhaps we have too few measurements?

simple.z.test(x, sd)
# [1]  7.559284 12.619890

#  ... 95% confidence interval
#      should be median +/- 1.96
#      you can see that we are a bit off!
mean(x)-7.56
# [1] 2.529587
12.62 - mean(x)
# [1] 2.530413


## Q 9.2
set.seed(9.2)

f <- function() mean(rnorm(15, mean=10, sd=5))
SE <- 5/sqrt(15)
xbar <- simple.sim(100, f)

alpha = 0.023;zstar = qnorm(1-alpha/2);sum(abs(xbar-10) < zstar*SE)
# [1] 94
alpha = 0.022;zstar = qnorm(1-alpha/2);sum(abs(xbar-10) < zstar*SE)
# [1] 96
alpha = 0.0225;zstar = qnorm(1-alpha/2);sum(abs(xbar-10) < zstar*SE)
# [1] 95

#  ... it appears that 97.75% is in the 95% confidence interval


## 9.3
set.seed(9.3)






