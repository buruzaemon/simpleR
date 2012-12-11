## Q 4.1
#  1. create separate tables for Q1 and Q2 
#  ... copying in directly from text, so
#      we will need to delete the leading column
#      for Students ...
tmp <- read.table(textConnection("1 3 5 1
2 3 2 3
3 3 5 1
4 4 5 1
5 3 2 1
6 4 2 3
7 3 5 1
8 4 5 1
9 3 4 1
10 4 2 1"))
#  ... transpose and add rownames, 
#      to make it easier to read ...
tmp <- t(tmp[-1])
rownames(tmp) <- c('Q1', 'Q2', 'Q3')

#  table for Q1
q1 <- factor(rep('Q1', 10))
r1 <- factor(tmp[1,], levels=c(1,2,3,4,5))
table.q1 <- table(q1, r1)
png(filename='images/4-1-1a.png')
barplot(table.q1, 
        main="Rankings for Q1",
	xlab="Rankings",
        col='darkblue', 
	legend.text=T)
dev.off()

#  table for Q2
q2 <- factor(rep('Q2', 10))
r2 <- factor(tmp[2,], levels=c(1,2,3,4,5))
table.q2 <- table(q2, r2)
png(filename='images/4-1-1b.png')
barplot(table.q2, 
        main="Rankings for Q2",
	xlab="Rankings",
        col='red', 
	legend.text=T)
dev.off()

#  2. create a contigency table
q1_2 <- factor(rep(c('Q1', 'Q2'), each=10))
r1_2 <- factor(append(tmp[1,], tmp[2,]), levels=c(1,2,3,4,5))
table(q1_2, r1_2)

#  3. stacked table of Q2 and Q3
png(filename='images/4-1-3.png')
q2_3 <- factor(rep(c('Q2', 'Q3'), each=10))
r2_3 <- factor(append(tmp[2,], tmp[3,]), levels=c(1,2,3,4,5))
table.q2_3 = table(q2_3, r2_3)
barplot(table.q2_3, 
        main="Q2 vs Q3",
	xlab="Rankings",
	col=c('red', 'darkgreen'),
	legend=T)
dev.off()

#  4. all Q's, side-by-side
png(filename='images/4-1-4.png')
q.all <- factor(rep(c('Q1', 'Q2', 'Q3'), each=10))
r.all <- factor(append(append(tmp[1,], tmp[2,]), tmp[3,]), levels=c(1,2,3,4,5))
table.all <- table(q.all, r.all)
barplot(table.all, 
        main="All Q's, Side-by-side",
	xlab="Rankings",
	col=c('darkblue', 'red', 'darkgreen'),
	legend=T,
	beside=T)
dev.off()


## Q 4.2
library(MASS)
data(package='MASS')
attach(UScereal)
summary(UScereal)
# mfr       calories        protein             fat            sodium     
# G:22   Min.   : 50.0   Min.   : 0.7519   Min.   :0.000   Min.   :  0.0  
# K:21   1st Qu.:110.0   1st Qu.: 2.0000   1st Qu.:0.000   1st Qu.:180.0  
# N: 3   Median :134.3   Median : 3.0000   Median :1.000   Median :232.0  
# P: 9   Mean   :149.4   Mean   : 3.6837   Mean   :1.423   Mean   :237.8  
# Q: 5   3rd Qu.:179.1   3rd Qu.: 4.4776   3rd Qu.:2.000   3rd Qu.:290.0  
# R: 5   Max.   :440.0   Max.   :12.1212   Max.   :9.091   Max.   :787.9  
#
# fibre            carbo           sugars          shelf      
# Min.   : 0.000   Min.   :10.53   Min.   : 0.00   Min.   :1.000  
# 1st Qu.: 0.000   1st Qu.:15.00   1st Qu.: 4.00   1st Qu.:1.000  
# Median : 2.000   Median :18.67   Median :12.00   Median :2.000  
# Mean   : 3.871   Mean   :19.97   Mean   :10.05   Mean   :2.169  
# 3rd Qu.: 4.478   3rd Qu.:22.39   3rd Qu.:14.00   3rd Qu.:3.000  
# Max.   :30.303   Max.   :68.00   Max.   :20.90   Max.   :3.000  
#
# potassium          vitamins 
# Min.   : 15.00   100%    : 5  
# 1st Qu.: 45.00   enriched:57  
# Median : 96.59   none    : 3  
# Mean   :159.12                
# 3rd Qu.:220.00                
# Max.   :969.70

df <- UScereal

#  1. The relationship between manufacturer and shelf
#     both are CATEGORIES
png(filename='images/4-2-1.png')
barplot(table(df$shelf, df$mfr), 
        main="Manufacturer & Shelf",
	xlab="Manufacturer",
	ylab="Shelf",
	col=rainbow(3),
	legend=T)
dev.off()

#  2. The relationship between fat and vitamins
#     fat is NUMERICAL, vitamains are CATEGORICAL
png(filename='images/4-2-2.png')
boxplot(fat ~ vitamins,
        main="Vitamins & Fat",
	xlab="Vitamins",
	ylab="Fat")
dev.off()

#  3. the relationship between fat and shelf
#     fat is NUMERICAL, shelf if CATEGORICAL
png(filename='images/4-2-3.png')
boxplot(fat ~ shelf,
        main="Shelf & Fat",
	xlab="Shelf",
	ylab="Fat")
dev.off()

#  4. the relationship between carbohydrates and sugars
#     both carbo and sugars are NUMERICAL, and possibly
#     with correlation?
#     plotting points and linear fit attempt to see
#     if there is a relation...
png(filename='images/4-2-4.png')
plot(carbo, sugars,
        main="Carbo & Sugars",
	xlab="Carbo",
	ylab="Sugars")
lm <- lm(sugars ~ carbo)
abline(lm, col="red")
dev.off()

#  5. the relationship between fibre and manufacturer
#      mfr is CATEGORICAL, fibre is NUMERICAL
png(filename='images/4-2-5.png')
boxplot(fibre ~ mfr,
        main="Mfr & Fibre",
	xlab="Mfr",
	ylab="Fibre")
dev.off()

#  6. the relationship between sodium and sugars
#     sodium and sugars are both NUMERICAL
#     plotting points and linear fit to see if there
#     is some relation
png(filename='images/4-2-6.png')
plot(sugars, sodium,
     main="Sugars & Sodium",
     xlab="Sugars",
     ylab="Sodium")
lm <- lm(sodium ~ sugars)
abline(lm, col="red")
dev.off()

#  7. Are there other relationships you can predict and investigate?
#     a. How about plotting Sugars to Calories?
png(filename='images/4-2-7a.png')
plot(sugars, calories,
     main="Sugars & Calories",
     xlab="Sugars",
     ylab="Calories")
lm <- lm(calories ~ sugars)
abline(lm, col="red")
dev.off()

#     a. How about plotting to Carbo to Fat?
png(filename='images/4-2-7b.png')
plot(carbo, fat,
     main="Carbo & Fat",
     xlab="Carbo",
     ylab="Fat")
lm <- lm(fat ~ carbo)
abline(lm, col="red")
dev.off()

detach(UScereal)


## Q 4.3
data(package='MASS', mammals)
attach(mammals)
summary(mammals)
#      body              brain        
# Min.   :   0.005   Min.   :   0.14  
# 1st Qu.:   0.600   1st Qu.:   4.25  
# Median :   3.342   Median :  17.25  
# Mean   : 198.790   Mean   : 283.13  
# 3rd Qu.:  48.203   3rd Qu.: 166.00  
# Max.   :6654.000   Max.   :5712.00 

#  1. compare Pearson and Spearman correlation
#     ... yes, they appear to be similar...
cor(body, brain, method='pearson')
# [1] 0.9341638

cor(body, brain, method='spearman')
# [1] 0.9534986

#  2. plot 
png(filename='images/4-3-2.png')
plot(body, brain,
     main="Body & Brain",
     xlab="body",
     ylab="brain")
lm.a <- lm(brain ~ body)
abline(lm.a, col="red")
dev.off()

summary(lm.a)
#
# Call:
# lm(formula = brain ~ body)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -810.07  -88.52  -79.64  -13.02 2050.33 
#
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 91.00440   43.55258    2.09   0.0409 *  
# body         0.96650    0.04766   20.28   <2e-16 ***
# ---
# Signif. codes:  0 e***f 0.001 e**f 0.01 e*f 0.05 e.f 0.1 e f 1 
#
# Residual standard error: 334.7 on 60 degrees of freedom
# Multiple R-squared: 0.8727,     Adjusted R-squared: 0.8705 
# F-statistic: 411.2 on 1 and 60 DF,  p-value: < 2.2e-16 

#  3. plot with logarithms of variables
png(filename='images/4-3-3.png')
plot(log1p(body), log1p(brain),
     main="Body & Brain, log values",
     xlab="log1p(body)",
     ylab="log1p(brain)")
lm.b <- lm(log1p(brain) ~ log1p(body))
abline(lm.b, col="red")
dev.off()

summary(lm.b)
#
# Call:
# lm(formula = log1p(brain) ~ log1p(body))
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.27379 -0.57842 -0.04617  0.40597  2.05871 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.40033    0.14208   9.856 3.69e-14 ***
# log1p(body)  0.89959    0.04578  19.652  < 2e-16 ***
# ---
# Signif. codes:  0 e***f 0.001 e**f 0.01 e*f 0.05 e.f 0.1 e f 1 
#
# Residual standard error: 0.7927 on 60 degrees of freedom
# Multiple R-squared: 0.8655,     Adjusted R-squared: 0.8633 
# F-statistic: 386.2 on 1 and 60 DF,  p-value: < 2.2e-16 

detach(mammals)


## Q 4.4
data(package='UsingR', homedata)
attach(homedata)

summary(homedata)
#     y1970            y2000        
# Min.   :     0   Min.   :   7400  
# 1st Qu.: 57000   1st Qu.: 161400  
# Median : 68900   Median : 251700  
# Mean   : 70821   Mean   : 268370  
# 3rd Qu.: 80500   3rd Qu.: 335600  
# Max.   :297200   Max.   :1182800

#  1. plot indicates a strong correlation 
#     linear model fits well
#     there are some outliers, but neglible
#     outliers may be luxury homes in trending 
#     neighborhoods?
png(filename='images/4-4-1.png')
plot(y1970, y2000,
     main="Home Prices: 1970 & 2000",
     xlab="1970",
     ylab="2000")
lm.c <- lm(y2000 ~ y1970)
abline(lm.c, col="red")
dev.off()

summary(lm.c)
#
# Call:
# lm(formula = y2000 ~ y1970)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -416665  -36308     809   34372  536605 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.040e+05  2.337e+03  -44.51   <2e-16 ***
# y1970        5.258e+00  3.147e-02  167.07   <2e-16 ***
# ---
# Signif. codes:  0 e***f 0.001 e**f 0.01 e*f 0.05 e.f 0.1 e f 1 
# 
# Residual standard error: 58000 on 6839 degrees of freedom
# Multiple R-squared: 0.8032,     Adjusted R-squared: 0.8032 
# F-statistic: 2.791e+04 on 1 and 6839 DF,  p-value: < 2.2e-16

#  2. predict price home selling for 75000 in 1970
b <- coef(lm.c)[[1]]
b
m <- coef(lm.c)[[2]]
m
predicted.price <- 75000*m + b
predicted.price

detach(homedata)
