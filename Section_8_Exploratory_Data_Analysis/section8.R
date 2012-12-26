## Q 8.1
data(package="UsingR", babies)
attach(babies)

# summary(babies)
#  ... NOTE: there is NO column named 'bwt',
#      using wt

boxplot(wt, horizontal=T, main="8.1 - wt: symmetric, long")
dev.copy(png, "images/8-1a.png")
dev.off()

boxplot(gestation, horizontal=T, main="8.1 - gestation: skewed, long")
dev.copy(png, "images/8-1b.png")
dev.off()

boxplot(age, horizontal=T, main="8.1 - age: skewed, regular")
dev.copy(png, "images/8-1c.png")
dev.off()

boxplot(ht, horizontal=T, main="8.1 - ht: symmetric, regular")
dev.copy(png, "images/8-1d.png")
dev.off()

#      using wt1
boxplot(wt1, horizontal=T, main="8.1 - wt1: skewed, long")
dev.copy(png, "images/8-1e.png")
dev.off()

detach(babies)

## Q 8.2
data(package="UsingR", iq)

summary(iq)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   72.0    93.0   101.0   101.4   109.2   130.0 

boxplot(iq, horizontal=T, main="8.2 - iq: symmetric, regular")
dev.copy(png, "images/8-2.png")
dev.off()

#  ... given this plot, the mean should be used for analyess
#      on iq


## Q 8.3
data(package="UsingR", slc)

summary(slc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0730  0.1920  0.2500  0.2632  0.3153  0.6230 

boxplot(slc, horizontal=T, main="8.3 - slc: skewed, regular")
dev.copy(png, "images/8-3.png")
dev.off()

#  ... given this plot, the median should be used for analyess
#      on slc


## Q 8.4
set.seed(8.4)
par(mfrow=c(3,1), oma=c(0,0,2,0))
boxplot(rt(100,2), horizontal=T, main="rt(100, df=2)")
boxplot(rt(100,10), horizontal=T, main="rt(100, df=10)")
boxplot(rt(100,25), horizontal=T, main="rt(100, df=25)")
title("8.4 - Comparison of t-distribution and DF", outer=T)
dev.copy(png, "images/8-4.png")
dev.off()

#  ... Student's t-distribution appears to be slightly skewed for
#      lower df, approaching a more symmetric distribution at
#      higer df values
#      t-distribution is sometimes used in place of normal
#      when the real data includes heavy tails: robust parametric
#      modelling...


## 8.5
set.seed(8.5)
par(mfrow=c(3,1), oma=c(0,0,2,0))
boxplot(rchisq(100,2), horizontal=T, main="rchisq(100, df=2)")
boxplot(rchisq(100,10), horizontal=T, main="rchisq(100, df=10)")
boxplot(rchisq(100,25), horizontal=T, main="rchisq(100, df=25)")
title("8.5 - Comparison of Chi-squared distribution and DF", outer=T)
dev.copy(png, "images/8-5.png")
dev.off()
par(mfrow=c(1,1))

#  ... Like Student's t, Chi-squared appears to be skewed for
#      lower df, approaching a more symmetric distribution at
#      higer df values


## 8.6
data(trees)
summary(trees)
#      Girth           Height       Volume     
#  Min.   : 8.30   Min.   :63   Min.   :10.20  
#  1st Qu.:11.05   1st Qu.:72   1st Qu.:19.40  
#  Median :12.90   Median :76   Median :24.20  
#  Mean   :13.25   Mean   :76   Mean   :30.17  
#  3rd Qu.:15.25   3rd Qu.:80   3rd Qu.:37.30  
#  Max.   :20.60   Max.   :87   Max.   :77.00  

boxplot(Girth, horizontal=T, main="8.6 - Girth: skewed, regular")
dev.copy(png, "images/8-6a.png")
dev.off()

boxplot(Height, horizontal=T, main="8.6 - Height: slightly-skewed, regular")
dev.copy(png, "images/8-6b.png")
dev.off()

boxplot(Volume, horizontal=T, main="8.6 - Volume: skewed, regular")
dev.copy(png, "images/8-6c.png")
dev.off()


## 8.7
data(dowdata)
summary(dowdata)
#        Date          Open            High            Low       
#  1-Apr-99:  1   Min.   : 9118   Min.   : 9268   Min.   : 8994  
#  1-Aug-00:  1   1st Qu.:10402   1st Qu.:10602   1st Qu.:10217  
#  1-Dec-99:  1   Median :10714   Median :10890   Median :10553  
#  1-Feb-00:  1   Mean   :10609   Mean   :10777   Mean   :10448  
#  1-Feb-99:  1   3rd Qu.:11008   3rd Qu.:11172   3rd Qu.:10845  
#  1-Jul-99:  1   Max.   :11719   Max.   :11908   Max.   :11506  
#  (Other) :437                                                  
#  
#       Close      
#   Min.   : 9121  
#   1st Qu.:10401  
#   Median :10719  
#   Mean   :10609  
#   3rd Qu.:11006  
#   Max.   :11723  

x <- dowdata$Close
n <- length(x)
z <- log(x[2:n]/x[1:(n-1)])

par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(x, main="hist(x)")
boxplot(x, main="boxplot(x)")
qqnorm(x, main="qqnorm(x)") 
qqline(x, col="red")
title("8.7 - dowdata: w/out logarithmic transformation", outer=T)
dev.copy(png, "images/8-7a.png")
dev.off()

par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(z, main="hist(z)")
boxplot(z, main="boxplot(z)")
qqnorm(z, main="qqnorm(z)") 
qqline(z, col="red")
title("8.7 - dowdata: w/ logarithmic transformation", outer=T)
dev.copy(png, "images/8-7b.png")
dev.off()


## 8.8
set.seed(8.8)

results <- c()
for (i in 1:200) results[i]=length(simple.chutes(sim=T))

#  ... results are skewed, long-tailed,
#      but you must see for yourself
par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(results, main="hist(results)")
boxplot(results, main="boxplot(results)")
qqnorm(results, main="qqnorm(results)")
qqline(results, col="red")
title("8.8 - Chutes & Ladders: histogram, boxplot & normal plot", outer=T)
dev.copy(png, "images/8-8a.png")
dev.off()

#  ... percentage of results > 100?
sum(results>100)/n
# [1] 0.02708804

#  ... compare median and mean
summary(results)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.00   22.00   37.00   44.12   56.25  174.00 

par(mfrow=c(1,1))
plot(simple.chutes(1), main="8.8 - Trajectory of Chutes & Ladders")
dev.copy(png, "images/8-8b.png")
dev.off()

