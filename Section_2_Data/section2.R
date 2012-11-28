# Ex. 2.1
#  You can read in a sequence of space-separated values with scan
#  miles <- scan()
miles <- c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
x <- diff(miles)

max(x)
# [1] 324

mean(x)
# [1] 305.1429

min(x)
# [1] 280

# Ex. 2.2
commutes <- c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)

max(commutes)
# [1] 24

mean(commutes)
# [1] 18.9

min(commutes)
# [1] 15

# Oops, the 24 was a mistake
# It should have been 18
commutes[commutes==24]=18
commutes
# [1] 17 16 20 18 22 15 21 15 17 22

mean(commutes)
# [1] 18.3

sum(commutes >= 20)
# [1] 4

n <- length(commutes)
l <- length(which(commutes<17))
l/n
# [1] 0.3


# Ex. 2.3
bill <- c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)

sum(bill)
# [1] 473

min(bill)
# [1] 30

max(bill)
# [1] 49

length(which(bill<40))
# [1] 7

n <- length(bill)
l <- length(which(bill<40))
l/n
# [1] 0.5833333


# Ex. 2.4
prices <- c(9000, 9500, 9400, 9400, 10000, 9500, 10300, 10200)

# The average price is much higher than 9500 USD
mean(prices)
# [1] 9662.5

min(prices)
# [1] 9000

max(prices)
# [1] 10300


# Ex. 2.5
# Given:
#   x = c(1,3,5,7,9)
#   y = c(2,3,5,7,11,13)
#
# 1. x+1
# 2. y*2
# ... operations are mapped to each vector element
# 3. length(x) and length(y)
# ... returns the respective vector lengths
# 4. x + y
# .... returns warning that the vectors are of different lengths
# 5. sum(x>5) and sum(x[x>5])
# ... first will return the number of elements > 5
# ... latter will return sum of elements > 5
# 6. sum(x>5 | x< 3) # read | as 'or', & and 'and'
# ... number of elements > 5 and < 3
# 7. y[3]
# ... 3rd element of y
# 8. y[-3]
# ... all elements of y except 3rd one
# 9. y[x] (What is NA?)
# ... returns elements of y with indices as listed
#     as the elements of x, but the indices beyond
#     the range of y are NA (or null)
# 10. y[y>=7]
# ... returns all elements of y >= 7


# Ex. 2.6
x <- c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5)

# 1.
sum(x)/10
# [1] 4.8

# 2.
log(x, base=10)
# [1] 0.0000000 0.9030900 0.3010300 0.7781513 0.4771213 0.9030900 0.6989700
# [8] 0.6989700 0.6989700 0.6989700
# ... log10(x) also works

# 3.
(x-4.4)/2.875
# [1] -1.1826087  1.2521739 -0.8347826  0.5565217 -0.4869565  1.2521739
# [7]  0.2086957  0.2086957  0.2086957  0.2086957

# 4.
diff(range(x))
# [1] 7

