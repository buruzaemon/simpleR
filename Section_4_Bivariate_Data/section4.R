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
abline(lm)
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
abline(lm)
dev.off()

#  7. Are there other relationships you can predict and investigate?
#     a. How about plotting Sugars to Calories?
png(filename='images/4-2-7a.png')
plot(sugars, calories,
     main="Sugars & Calories",
     xlab="Sugars",
     ylab="Calories")
lm <- lm(calories ~ sugars)
abline(lm)
dev.off()

#     a. How about plotting to Carbo to Fat?
png(filename='images/4-2-7b.png')
plot(carbo, fat,
     main="Carbo & Fat",
     xlab="Carbo",
     ylab="Fat")
lm <- lm(fat ~ carbo)
abline(lm)
dev.off()

