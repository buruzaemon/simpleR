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
