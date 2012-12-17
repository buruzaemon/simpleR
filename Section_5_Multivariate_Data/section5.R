## Q 5.1
data(emissions)
summary(emissions)
#      GDP            perCapita          CO2        
# Min.   :  59900   Min.   : 2507   Min.   :  54.0  
# 1st Qu.: 123100   1st Qu.:13393   1st Qu.:  77.0  
# Median : 206250   Median :20993   Median : 200.0  
# Mean   : 830427   Mean   :17724   Mean   : 669.4  
# 3rd Qu.: 683500   3rd Qu.:22250   3rd Qu.: 547.5  
# Max.   :8083000   Max.   :29647   Max.   :6750.0  

png(filename="images/5-1-1.png")
boxplot(emissions,
  main="5.1 - emissions, w/ outlier")
# guess who the single outlier is???
#identify(emissions$GDP, n=1)
#[1] 1
dev.off()

png(filename="images/5-1-2.png")
boxplot(emissions[-1,],
  main="5.1 - emissions w/out outlier")
dev.off()


