library(UsingR)

## Q 9.1
#  ... assuming that sigma is known
#      and the X_i are normally distributed...
set.seed(9.1)
mu = 10
sd = 5

x <- rnorm(15, mu, sd)

#  ... let's just visually examine what we've got...
#      looks to be fairly normal, in spite of the
#      small number of samples (15)
simple.eda(x)
dev.copy(png, "images/9-1.png")
dev.off()

xbar <- mean(x)
se   <- sd/sqrt(length(x))

z <- (xbar - mu) / se
z
#[1] 0.06939382

#  ... and from a standard normal table,
#      we see that this measurement is roughly
#      only 2.8% of the population have sample mean
#      this close away from the population mean (10)
#      i.e., this particular 1-sample z test got it right


## Q 9.2
set.seed(9.2)

f <- function() mean(rnorm(15, mean=10, sd=5))
SE <- 5/sqrt(15)
xbar <- simple.sim(100, f)

alpha = 0.1;  zstar = qnorm(1-alpha/2); sum(abs(xbar-10) < zstar*SE)
# [1] 87
alpha = 0.05; zstar = qnorm(1-alpha/2); sum(abs(xbar-10) < zstar*SE)
# [1] 93
alpha = 0.01; zstar = qnorm(1-alpha/2); sum(abs(xbar-10) < zstar*SE)
# [1] 98

#  ... it appears that 93% of our samples calculated above
#      are in the 95% confidence interval


## 9.3
t.test(x)
#
#        One Sample t-test
#
# data:  x 
# t = 9.2971, df = 14, p-value = 2.286e-07
# alternative hypothesis: true mean is not equal to 0 
# 95 percent confidence interval:
#   7.761966 12.417208 
# sample estimates:
# mean of x 
#  10.08959 

t.test(xbar)
#
#        One Sample t-test
#
# data:  xbar 
# t = 71.6056, df = 99, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0 
# 95 percent confidence interval:
#   9.818487 10.378143 
# sample estimates:
# mean of x 
#  10.09831 

#  ... this looks to be good, too
#      note how when increasing n, the 
#      ??? 


## 9.4
data(package='UsingR', exec.pay)

wilcox.test(exec.pay, conf.int=T, conf.level=.8)
#
#        Wilcoxon signed rank test with continuity correction
#
# data:  exec.pay 
# V = 19306, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0 
# 80 percent confidence interval:
#  27.00005 31.49996 
# sample estimates:
# (pseudo)median 
#       29.00002 

wilcox.test(exec.pay, conf.int=T, conf.level=.95)
#
#        Wilcoxon signed rank test with continuity correction
#
# data:  exec.pay 
# V = 19306, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0 
# 95 percent confidence interval:
#  25.99998 32.99994 
# sample estimates:
# (pseudo)median 
#       29.00002 


## 9.5
data(package='UsingR', rat)

simple.eda(rat)
dev.copy(png, "images/9-5.png")
dev.off()

## ... i think this small data set looks normal enough to go

t.test(rat)
#
#        One Sample t-test
#
# data:  rat 
# t = 14.1763, df = 19, p-value = 1.48e-11
# alternative hypothesis: true mean is not equal to 0 
# 95 percent confidence interval:
#  96.69997 130.20003 
# sample estimates:
# mean of x 
#    113.45 

#  ... let's compare SE...
(130.20003 - 96.69997)/4
# [1] 8.375015

sd(rat) / sqrt(length(rat))
# [1] 8.002787

#  ... we are at 0.956 % accuracy!


## 9.6
data(package='UsingR', puerto)

simple.eda(puerto)
dev.copy(png, "images/9-6.png")
dev.off()

#  ... ok, now this guy's Normal plot suggests
#      that this is not really normal?!
simple.eda(puerto)
dev.copy(png, "images/9-6.png")
dev.off()

#  ... looks pretty normal, actually!

t.test(puerto)
#
#        One Sample t-test
#
# data:  puerto 
# t = 25.8466, df = 49, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0 
# 95 percent confidence interval:
#  255.9244 299.0756 
# sample estimates:
# mean of x 
#     277.5


## 9.7
data(package='UsingR', malpract)

simple.eda(malpract)
dev.copy(png, "images/9-7.png")
dev.off()

#  ... for median, we use wilcox.test
#      if this data is not normally distributed,
#      the median is preferable to the mean
#      see how the Normal plot is way out of line?
#      and how the boxplot is skewed to the higher values?p

wilcox.test(malpract, conf.int=T, conf.level=.9)
#
#        Wilcoxon signed rank test
#
# data:  malpract 
# V = 153, p-value = 1.526e-05
# alternative hypothesis: true location is not equal to 0 
# 90 percent confidence interval:
#   385 1325 
# sample estimates:
# (pseudo)median 
#            800 



