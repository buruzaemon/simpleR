## Q 5.1
data(package="UsingR", emissions)
summary(emissions)
#      GDP            perCapita          CO2        
# Min.   :  59900   Min.   : 2507   Min.   :  54.0  
# 1st Qu.: 123100   1st Qu.:13393   1st Qu.:  77.0  
# Median : 206250   Median :20993   Median : 200.0  
# Mean   : 830427   Mean   :17724   Mean   : 669.4  
# 3rd Qu.: 683500   3rd Qu.:22250   3rd Qu.: 547.5  
# Max.   :8083000   Max.   :29647   Max.   :6750.0  

attach(emissions)
png(filename="images/5-1-1.png")
plot(GDP, CO2,
  main="5.1 - emissions, w/ outlier")
# guess who the single outlier is???
#identify(locator(1), n=1)
#[1] 1
dev.off()

png(filename="images/5-1-2.png")
plot(GDP[-1], CO2[-1],
  main="5.1 - emissions w/out outlier")
dev.off()
detach(emissions)


## Q 5.2
data(package="UsingR", chips)
summary(chips)
#     wafer11          wafer12        wafer13        wafer14      
#  Min.   : 840.0   Min.   : 850   Min.   : 830   Min.   : 850.0  
#  1st Qu.: 982.5   1st Qu.: 960   1st Qu.: 970   1st Qu.: 972.5  
#  Median :1030.0   Median :1020   Median :1020   Median :1005.0  
#  Mean   :1014.7   Mean   :1009   Mean   :1014   Mean   :1010.7  
#  3rd Qu.:1050.0   3rd Qu.:1060   3rd Qu.:1060   3rd Qu.:1060.0  
#  Max.   :1120.0   Max.   :1120   Max.   :1120   Max.   :1120.0  
#     wafer21          wafer22          wafer23        wafer24    
#  Min.   : 840.0   Min.   : 900.0   Min.   : 910   Min.   : 900  
#  1st Qu.: 992.5   1st Qu.: 982.5   1st Qu.: 980   1st Qu.: 990  
#  Median :1020.0   Median :1025.0   Median :1015   Median :1020  
#  Mean   :1016.0   Mean   :1024.7   Mean   :1020   Mean   :1021  
#  3rd Qu.:1060.0   3rd Qu.:1070.0   3rd Qu.:1068   3rd Qu.:1060  
#  Max.   :1120.0   Max.   :1130.0   Max.   :1110   Max.   :1140 

png(filename="images/5-2-1.png")
boxplot(chips, main="5.2 - chips")
dev.off()

library(UsingR)
png(filename="images/5-2-2.png")
simple.densityplot(chips)
dev.off()
#  slight variations on mean, but they all seem to be similar


## Q 5.3
data(package="UsingR", chicken)
summary(chicken)
#     Ration1         Ration2     Ration3     
#  Min.   :2.000   Min.   :3   Min.   :5.000  
#  1st Qu.:3.000   1st Qu.:4   1st Qu.:6.000  
#  Median :4.000   Median :5   Median :6.000  
#  Mean   :4.154   Mean   :5   Mean   :6.385  
#  3rd Qu.:5.000   3rd Qu.:6   3rd Qu.:7.000  
#  Max.   :7.000   Max.   :7   Max.   :8.000  
 
png(filename="images/5-3.png")
boxplot(chicken, main="5.3 - chicken")
dev.off()
#  there is a clear difference in ave. weight by ration


## Q 5.4
library(MASS)
data(package="UsingR", kid.weights)
summary(kid.weights)
#       age             weight           height      gender 
#  Min.   :  3.00   Min.   : 10.00   Min.   :12.00   F:129  
#  1st Qu.: 12.25   1st Qu.: 22.00   1st Qu.:28.00   M:121  
#  Median : 39.00   Median : 32.00   Median :36.00          
#  Mean   : 47.95   Mean   : 38.38   Mean   :36.52          
#  3rd Qu.: 69.75   3rd Qu.: 45.00   3rd Qu.:43.00          
#  Max.   :144.00   Max.   :150.00   Max.   :67.00 

attach(kid.weights)
age.yr = cut(age, seq(0,144,by=12), labels=0:11)

png(filename="images/5-4.png")
boxplot(weight ~ age.yr, main="5.4 - age vs. weights")
dev.off()
detach(kid.weights)
#  a clear upward trend in weight, with the range increasing as 
#  the child gets older


## Q 5.5
data(package='UsingR', carbon)
summary(carbon)
#     Monoxide           Site  
#  Min.   :0.1050   Min.   :1  
#  1st Qu.:0.1087   1st Qu.:1  
#  Median :0.1170   Median :2  
#  Mean   :0.1172   Mean   :2  
#  3rd Qu.:0.1212   3rd Qu.:3  
#  Max.   :0.1420   Max.   :3  

attach(carbon)
png(filename="images/5-5.png")
boxplot(Monoxide ~ Site, main="5.5 - Monoxide by Site")
dev.off()
detach(carbon)
#  Site 2 seems to have clearly higher levels than the
#  other 2, which are both at the same level


## Q 5.6
data(package='UsingR', babies)
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

attach(babies)
png(filename='images/5-6-1.png', width=960, height=960)
pairs(babies)
dev.off()

#  ... now, looking at this pair-wise comparison, 
#      which seem to have a linear relation?
png(filename='images/5-6-2a.png')
plot(parity, age, 
    main="5.6 - babies: Linear Relations?")
abline(lm(age ~ parity), col='red')
dev.off()

png(filename='images/5-6-2b.png')
plot(age, dage, 
    main="5.6 - babies: Linear Relations?")
abline(lm(dage ~ age), col='red')
dev.off()

#  ... make a scatter plot of birthweight and gestation,
#      using different pch for the level of 'factor' smoke
#      (smoke is not really a factor, but numeric!!!)...
plot(wt ~ gestation, 
    main="5.6 - babies: gestation vs weight")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
tmp = levels(factor(smoke))
points(wt ~ gestation, 
    pch=smoke, 
    col=heat.colors(n=length(tmp)))
legend(locator(1), 
    title="Factor: smoke",
    cex=0.8,
    legend=tmp, 
    pch=smoke, 
    col=heat.colors(n=length(tmp)),
    horiz=T)
dev.copy(png, "images/5-6-2c.png")
dev.off()
