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
        main="4.1 - Rankings for Q1",
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
        main="4.1 - Rankings for Q2",
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
        main="4.1 - Q2 vs Q3",
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
        main="4.1 - All Q's, Side-by-side",
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
        main="4.2 - Manufacturer & Shelf",
	xlab="Manufacturer",
	ylab="Shelf",
	col=rainbow(3),
	legend=T)
dev.off()

#  2. The relationship between fat and vitamins
#     fat is NUMERICAL, vitamains are CATEGORICAL
png(filename='images/4-2-2.png')
boxplot(fat ~ vitamins,
        main="4.2 - Vitamins & Fat",
	xlab="Vitamins",
	ylab="Fat")
dev.off()

#  3. the relationship between fat and shelf
#     fat is NUMERICAL, shelf if CATEGORICAL
png(filename='images/4-2-3.png')
boxplot(fat ~ shelf,
        main="4.2 - Shelf & Fat",
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
        main="4.2 - Carbo & Sugars",
	xlab="Carbo",
	ylab="Sugars")
lm <- lm(sugars ~ carbo)
abline(lm, col="red")
dev.off()

#  5. the relationship between fibre and manufacturer
#      mfr is CATEGORICAL, fibre is NUMERICAL
png(filename='images/4-2-5.png')
boxplot(fibre ~ mfr,
        main="4.2 - Mfr & Fibre",
	xlab="Mfr",
	ylab="Fibre")
dev.off()

#  6. the relationship between sodium and sugars
#     sodium and sugars are both NUMERICAL
#     plotting points and linear fit to see if there
#     is some relation
png(filename='images/4-2-6.png')
plot(sugars, sodium,
     main="4.2 - Sugars & Sodium",
     xlab="Sugars",
     ylab="Sodium")
lm <- lm(sodium ~ sugars)
abline(lm, col="red")
dev.off()

#  7. Are there other relationships you can predict and investigate?
#     a. How about plotting Sugars to Calories?
png(filename='images/4-2-7a.png')
plot(sugars, calories,
     main="4.2 - Sugars & Calories",
     xlab="Sugars",
     ylab="Calories")
lm <- lm(calories ~ sugars)
abline(lm, col="red")
dev.off()

#     a. How about plotting to Carbo to Fat?
png(filename='images/4-2-7b.png')
plot(carbo, fat,
     main="4.2 - Carbo & Fat",
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
     main="4.3. - Body & Brain",
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
# Signif. codes:  0 Åe***Åf 0.001 Åe**Åf 0.01 Åe*Åf 0.05 Åe.Åf 0.1 Åe Åf 1 
#
# Residual standard error: 334.7 on 60 degrees of freedom
# Multiple R-squared: 0.8727,     Adjusted R-squared: 0.8705 
# F-statistic: 411.2 on 1 and 60 DF,  p-value: < 2.2e-16 

#  3. plot with logarithms of variables
png(filename='images/4-3-3.png')
plot(log1p(body), log1p(brain),
     main="4.3 - Body & Brain, log values",
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
# Signif. codes:  0 Åe***Åf 0.001 Åe**Åf 0.01 Åe*Åf 0.05 Åe.Åf 0.1 Åe Åf 1 
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
     main="4.4 - Home Prices: 1970 & 2000",
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
# Signif. codes:  0 Åe***Åf 0.001 Åe**Åf 0.01 Åe*Åf 0.05 Åe.Åf 0.1 Åe Åf 1 
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


## Q 4.5
data(package='UsingR', 'florida')
attach(florida)

names(florida)
# [1] "County"     "GORE"       "BUSH"       "BUCHANAN"   "NADER"     
# [6] "BROWN"      "HAGELIN"    "HARRIS"     "MCREYNOLDS" "MOOREHEAD" 
# [11] "PHILLIPS"   "Total"     

summary(florida)
#      County        GORE             BUSH           BUCHANAN     
# ALACHUA : 1   Min.   :   788   Min.   :  1316   Min.   :   9.0  
# BAKER   : 1   1st Qu.:  3055   1st Qu.:  4746   1st Qu.:  46.5  
# BAY     : 1   Median : 14152   Median : 20196   Median : 114.0  
# BRADFORD: 1   Mean   : 43341   Mean   : 43356   Mean   : 258.5  
# BREVARD : 1   3rd Qu.: 45974   3rd Qu.: 56542   3rd Qu.: 285.5  
# BROWARD : 1   Max.   :386518   Max.   :289456   Max.   :3407.0  
# (Other) :61                                                     
#     NADER          BROWN           HAGELIN          HARRIS      
# Min.   :  19   Min.   :   4.0   Min.   :  0.0   Min.   :   0.0  
# 1st Qu.:  95   1st Qu.:  23.5   1st Qu.:  3.0   1st Qu.:   1.0  
# Median : 562   Median : 116.0   Median : 13.0   Median :   4.0  
# Mean   :1441   Mean   : 280.7   Mean   : 34.1   Mean   : 156.3  
# 3rd Qu.:1871   3rd Qu.: 321.5   3rd Qu.: 33.5   3rd Qu.:   8.5  
# Max.   :9986   Max.   :3211.0   Max.   :444.0   Max.   :9888.0  
#                                                                      
#   MCREYNOLDS       MOOREHEAD         PHILLIPS           Total       
# Min.   :  0.00   Min.   :  0.00   Min.   :   0.00   Min.   :  2403  
# 1st Qu.:  1.00   1st Qu.:  4.00   1st Qu.:   3.00   1st Qu.:  8080  
# Median :  3.00   Median : 12.00   Median :  10.00   Median : 34941  
# Mean   : 19.03   Mean   : 27.45   Mean   :  63.82   Mean   : 88978  
# 3rd Qu.:  5.00   3rd Qu.: 32.00   3rd Qu.:  20.50   3rd Qu.:102874  
# Max.   :658.00   Max.   :167.00   Max.   :2927.00   Max.   :625269

#  1. use the simple.lm function in the UsingR package to plot
#     and locate those 2 outliers...
library(UsingR)
simple.lm(BUSH,BUCHANAN)
#
# Call:
# lm(formula = y ~ x)
#
# Coefficients:
# (Intercept)            x  
#   45.289861     0.004917  

## NOTE
#  you would have to click on the active graphics 
#  window on the 2 outlier points to obtain their
#  indices
#identify(BUSH, BUCHANAN, n=2)
# [1] 13 50

BUSH[13]
# [1] 289456

BUCHANAN[13]
# [1] 561

BUSH[50]
# [1] 152846

BUCHANAN[50]
# [1] 3407

florida[13,]
#   County   GORE   BUSH BUCHANAN NADER BROWN HAGELIN HARRIS MCREYNOLDS MOOREHEAD PHILLIPS  Total
#13   DADE 328702 289456      561  5355   759     119     88         36       124       69 625269

florida[50,]
#       County   GORE   BUSH BUCHANAN NADER BROWN HAGELIN HARRIS MCREYNOLDS MOOREHEAD PHILLIPS  Total
#50 PALM BEACH 268945 152846     3407  5564   743     143     45        302       103      188 432286

df <- florida[(County!='DADE' & County!='PALM BEACH'),]
simple.lm(df$BUSH, df$BUCHANAN, pred=BUSH[50])
#        1 
# 711.6168 
#
# ...
# Coefficients:
# (Intercept)            x  
#   38.536279     0.004404 

#  ... and even when removing these 2 outliers,
#      there doesn't seem to be appreciable difference
png(filename='images/4-5-1.png')
plot(BUSH, BUCHANAN,
    main="4.5 - Florida: Bush vs Buchanan",
    xlab="Bush",
    ylab="Buchanan")
abline(lm(BUCHANAN ~ BUSH), 
    col="red", 
    lty=1)
abline(lm(df$BUCHANAN ~ df$BUSH), 
    col="darkblue", 
    lty=2)
legend("topright", 
    c("w/ outliers", "w/o outliers"), 
    text.col=c('red', 'darkblue'), 
    col=c("red", "darkblue"), 
    lty=1:2,
    inset=0.05, 
    cex=0.8)
dev.off()

detach(florida)


## Q 4.6
data(package="UsingR", emissions)
attach(emissions)

summary(emissions)
#      GDP            perCapita          CO2        
# Min.   :  59900   Min.   : 2507   Min.   :  54.0  
# 1st Qu.: 123100   1st Qu.:13393   1st Qu.:  77.0  
# Median : 206250   Median :20993   Median : 200.0  
# Mean   : 830427   Mean   :17724   Mean   : 669.4  
# 3rd Qu.: 683500   3rd Qu.:22250   3rd Qu.: 547.5  
# Max.   :8083000   Max.   :29647   Max.   :6750.0 

png(filename='images/4-6-1.png')
plot(perCapita, CO2,
    main="4.6 - Emissions: Per-Capita vs CO2",
    xlab="perCapita",
    ylab="CO2")
abline(lm(CO2 ~ perCapita), 
    col="red", 
    lty=1)

#  1. Guess which country is the sole outlier?
#identify(perCapita, CO2, n=1)
#[1] 1

##emissions[1,]
##                  GDP perCapita  CO2
## UnitedStates 8083000     29647 6750

#  ... but even w/out this outlier, the regression line
#      doesn't much change (kind of sad, really)
abline(lm(emissions[-1]$CO2 ~ emissions[-1]$perCapita),
    col="darkblue", 
    lty=2)
legend("topleft", 
    c("w/ outlier", "w/o outlier"), 
    text.col=c('red', 'darkblue'), 
    col=c("red", "darkblue"), 
    lty=1:2,
    inset=0.05, 
    cex=0.8)
dev.off()

detach(emissions)


## Q 4.7
data(package="UsingR", babies)
attach(babies)

summary(babies)
#       id          pluralty    outcome       date        gestation          sex   
# Min.   :  15   Min.   :5   Min.   :1   Min.   :1350   Min.   :148.0   Min.   :1  
# 1st Qu.:5286   1st Qu.:5   1st Qu.:1   1st Qu.:1444   1st Qu.:272.0   1st Qu.:1  
# Median :6730   Median :5   Median :1   Median :1540   Median :280.0   Median :1  
# Mean   :6001   Mean   :5   Mean   :1   Mean   :1536   Mean   :286.9   Mean   :1  
# 3rd Qu.:7583   3rd Qu.:5   3rd Qu.:1   3rd Qu.:1627   3rd Qu.:288.0   3rd Qu.:1  
# Max.   :9263   Max.   :5   Max.   :1   Max.   :1714   Max.   :999.0   Max.   :1  
#       wt            parity            race             age              ed       
# Min.   : 55.0   Min.   : 0.000   Min.   : 0.000   Min.   :15.00   Min.   :0.000  
# 1st Qu.:108.8   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:23.00   1st Qu.:2.000  
# Median :120.0   Median : 1.000   Median : 3.000   Median :26.00   Median :2.000  
# Mean   :119.6   Mean   : 1.932   Mean   : 3.206   Mean   :27.37   Mean   :2.922  
# 3rd Qu.:131.0   3rd Qu.: 3.000   3rd Qu.: 7.000   3rd Qu.:31.00   3rd Qu.:4.000  
# Max.   :176.0   Max.   :13.000   Max.   :99.000   Max.   :99.00   Max.   :9.000  
#       ht             wt1          drace             dage            ded       
# Min.   :53.00   Min.   : 87   Min.   : 0.000   Min.   :18.00   Min.   :0.000  
# 1st Qu.:62.00   1st Qu.:115   1st Qu.: 0.000   1st Qu.:25.00   1st Qu.:2.000  
# Median :64.00   Median :126   Median : 3.000   Median :29.00   Median :4.000  
# Mean   :64.67   Mean   :154   Mean   : 3.665   Mean   :30.74   Mean   :3.189  
# 3rd Qu.:66.00   3rd Qu.:140   3rd Qu.: 7.000   3rd Qu.:35.00   3rd Qu.:5.000  
# Max.   :99.00   Max.   :999   Max.   :99.000   Max.   :99.00   Max.   :9.000  
#      dht             dwt           marital           inc            smoke       
# Min.   :60.00   Min.   :110.0   Min.   :0.000   Min.   : 0.00   Min.   :0.0000  
# 1st Qu.:70.00   1st Qu.:165.0   1st Qu.:1.000   1st Qu.: 2.00   1st Qu.:0.0000  
# Median :73.00   Median :190.0   Median :1.000   Median : 4.00   Median :1.0000  
# Mean   :81.67   Mean   :505.4   Mean   :1.038   Mean   :13.16   Mean   :0.8681  
# 3rd Qu.:99.00   3rd Qu.:999.0   3rd Qu.:1.000   3rd Qu.: 7.00   3rd Qu.:1.0000  
# Max.   :99.00   Max.   :999.0   Max.   :5.000   Max.   :98.00   Max.   :9.0000  
#     time            number      
# Min.   : 0.000   Min.   : 0.000  
# 1st Qu.: 0.000   1st Qu.: 0.000  
# Median : 1.000   Median : 1.000  
# Mean   : 1.748   Mean   : 2.604  
# 3rd Qu.: 1.000   3rd Qu.: 3.000  
# Max.   :99.000   Max.   :98.000 

cor(age, wt1, method="pearson")
# [1] 0.06273172
cor(age, wt1, method="spearman")
# [1] 0.1453316

lm.babies <- lm(wt1 ~ age)

# ... and plot shows this lack of correlation...
png(filename='images/4-7-1.png')
plot(age, wt1,
     main="4.7 - Babies: Age vs Weight",
     xlab="Age",
     ylab="Weight")
abline(lm.babies, col="red")
dev.off()

# let's just double-check this lack of correlation
png(filename='images/4-7-2.png')
par(mfcol=c(2,1))
plot(lm.babies, which=1)
plot(lm.babies, which=2)
dev.off()

summary(lm.babies)
# Call:
# lm(formula = wt1 ~ age)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -118.88  -39.13  -27.57  -12.13  857.05 
#
# Coefficients:
#        Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 114.6541    18.2974   6.266  5.1e-10 ***
# age           1.4366     0.6506   2.208   0.0274 *  
# ---
# Signif. codes:  0 Åe***Åf 0.001 Åe**Åf 0.01 Åe*Åf 0.05 Åe.Åf 0.1 Åe Åf 1 
#
# Residual standard error: 147.6 on 1234 degrees of freedom
# Multiple R-squared: 0.003935,   Adjusted R-squared: 0.003128 
# F-statistic: 4.875 on 1 and 1234 DF,  p-value: 0.02743

cor(ht, wt1, method="pearson")
# [1] 0.6010033
cor(ht, wt1, method="spearman")
# [1] 0.5036179

lm.babies <- lm(wt1 ~ ht)

# ... and again, plot shows lack of correlation...
png(filename='images/4-7-3.png')
plot(ht, wt1,
     main="4.7 - Babies: Height vs Weight",
     xlab="Height",
     ylab="Weight")
abline(lm.babies, col="red")
dev.off()

# double-check!
png(filename='images/4-7-4.png')
par(mfcol=c(2,1))
plot(lm.babies, which=1)
plot(lm.babies, which=2)
dev.off()

summary(lm.babies)
# Call:
# lm(formula = wt1 ~ ht)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -614.89  -40.28  -15.77   13.18  940.80 
#
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -938.4516    41.4926  -22.62   <2e-16 ***
# ht            16.8924     0.6395   26.41   <2e-16 ***
# ---
# Signif. codes:  0 Åe***Åf 0.001 Åe**Åf 0.01 Åe*Åf 0.05 Åe.Åf 0.1 Åe Åf 1 
#
# Residual standard error: 118.2 on 1234 degrees of freedom
# Multiple R-squared: 0.3612,     Adjusted R-squared: 0.3607 
#  F-statistic: 697.8 on 1 and 1234 DF,  p-value: < 2.2e-16 

detach(babies)


## Q 4.8
attach(women)
summary(women)
#     height         weight     
# Min.   :58.0   Min.   :115.0  
# 1st Qu.:61.5   1st Qu.:124.5  
# Median :65.0   Median :135.0  
# Mean   :65.0   Mean   :136.7  
# 3rd Qu.:68.5   3rd Qu.:148.0  
# Max.   :72.0   Max.   :164.0  

png(filename='images/4-8.png')
plot(height, weight,
     main="4.8 - Women: Height vs Weight",
     xlab="Height",
     ylab="Weight")
abline(lm(weight ~ height), col="red")
dev.off()

detach(women)


## Q 4.9
attach(mtcars)
summary(mtcars)
#      mpg             cyl             disp             hp       
# Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
# 1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
# Median :19.20   Median :6.000   Median :196.3   Median :123.0  
# Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
# 3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
# Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
#     drat             wt             qsec             vs        
# Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
# 1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
# Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
# Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
# 3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
# Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
#       am              gear            carb      
# Min.   :0.0000   Min.   :3.000   Min.   :1.000  
# 1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
# Median :0.0000   Median :4.000   Median :2.000  
# Mean   :0.4062   Mean   :3.688   Mean   :2.812  
# 3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
# Max.   :1.0000   Max.   :5.000   Max.   :8.000  

#  1. names?
names(mtcars)
# [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
# [11] "carb"

#  2. max mpg?
max(mtcars$mpg)
# [1] 33.9

#  3. and which car is it?
mtcars[mpg==max(mpg),]
#                 mpg cyl disp hp drat    wt qsec vs am gear carb
# Toyota Corolla 33.9   4 71.1 65 4.22 1.835 19.9  1  1    4    1

#  4. list first 5 cars 
mtcars[1:5,]
#                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2

#  5. Valiant's hp?
mtcars['Valiant',]$hp
# [1] 105

#  6. Merc 450LC?
mtcars['Merc 450SLC',]
#              mpg cyl  disp  hp drat   wt qsec vs am gear carb
# Merc 450SLC 15.2   8 275.8 180 3.07 3.78   18  0  0    3    3

#  7. cyl vs mpg?
png(filename='images/4-9.png')
plot(cyl, mpg,
     main="4.9 - Cars: cyl vs mpg",
     xlab="Cylinders",
     ylab="MPG")
abline(lm(mpg ~ cyl), col="red")
dev.off()

detach(mtcars)



