## Q 6.1
set.seed(6.1)
x <- runif(10, min=0, max=10)
x
# [1] 6.062683 9.376420 2.643521 3.800939 8.074834 9.780757 9.579337 7.627319
# [9] 5.096485 0.644768

min(x)
# [1] 0.644768

max(x)
# [1] 9.780757


## Q 6.2
set.seed(6.2)
x <- rnorm(10, mean=5, sd=5)
x
# [1]  6.348030  1.850073  9.343299 13.635978  5.120938  6.840126 -1.546021
# [8]  8.693110  5.224365 -0.241986

sum(x<0)
# [1] 2


## Q 6.3
set.seed(6.3)
m = 100
s = 10
x <- rnorm(100, mean=m, sd=s)

sum(x<m-2*s | x>m+2*s)
# [1] 5


## Q 6.4
set.seed(6.4)
n = 1
p = 0.5
x <- rbinom(50, n, p)

sum(x==1)
# [1] 30


## Q 6.5
#  ... assuming you mean a 6-sided die...
set.seed(6.5)
x <- sample(1:6, 100, replace=T)

sum(x==6)
# [1] 21


## Q 6.6
set.seed(6.6)
x <- sample(1:49, 6, replace=F)
x
# [1] 30 46 13 18 37 44

 min(x)
# [1] 13

max(x)
# [1] 46


## Q 6.7
#qnorm(0.05) 
qnorm(0.05, 0, 1) 
# [1] -1.644854


## Q 6.8
#  mean is 0, meaning this probability distribution
#  is symmetric about the y-axis.
#  for P(-z <= Z <= z) = 0.05...
#  => z where qnorm(0.5 + 0.025)
z <- qnorm(0.525)
z
# [1] 0.06270678

# pnorm(-z) = 0.5 - 0.025
pnorm(-1*z)
# [1] 0.475

# pnorm(z)  = 0.5 + 0.025
pnorm(z)
# [1] 0.525


## Q 6.9
pnorm(1.5, 0, 2, lower.tail=F)
# [1] 0.2266274


## 6.10
set.seed(6.10)
x <- rexp(100, 1/10)
hist(x, probability=T, col=gray(0.9), main="6.10 - exponential, mean=10")
curve(dexp(x, 1/10), col="red", add=T)
dev.copy(png, "images/6-10.png")
dev.off()

#  ... median?
median(x)
# [1] 5.709862


## 6.11
#  ... off the top of my head, it looks like rnorm(5, mean=0, sd=1:5)
#      will:
#      a. return 5 random numbers from a normal distribution centered about the Y-axis
#      b. with each random number coming from a normal distribution with sd set to 1-5
set.seed(6.11)
rnorm(5, mean=0, sd=1:5)
# [1]  0.2696060 -1.2599708  2.6059795  6.9087821  0.1209382


## 6.12
set.seed(6.12)
cards = paste(rep(c("A",2:10,"J","Q","K"),4),c("H","D","S","C"))
cards
# [1] "A H"  "2 D"  "3 S"  "4 C"  "5 H"  "6 D"  "7 S"  "8 C"  "9 H"  "10 D" "J S" 
# [12] "Q C"  "K H"  "A D"  "2 S"  "3 C"  "4 H"  "5 D"  "6 S"  "7 C"  "8 H"  "9 D" 
# [23] "10 S" "J C"  "Q H"  "K D"  "A S"  "2 C"  "3 H"  "4 D"  "5 S"  "6 C"  "7 H" 
# [34] "8 D"  "9 S"  "10 C" "J H"  "Q D"  "K S"  "A C"  "2 H"  "3 D"  "4 S"  "5 C" 
# [45] "6 H"  "7 D"  "8 S"  "9 C"  "10 H" "J D"  "Q S"  "K C" 

sample(cards, 5)
# [1] "6 C" "9 C" "A D" "6 S" "K S"
sample(cards, 5)
# [1] "Q S"  "10 H" "K S"  "Q H"  "4 C" 

#  ... it only took 2 tries before getting a pair of queens!
