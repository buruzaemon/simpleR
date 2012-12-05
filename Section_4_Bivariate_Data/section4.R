## Q 4.1
#  1. create separate tables for Q1 and Q2 
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
tmp <- tmp[-1]
table.Q1 <- tmp[1]
colnames(table.Q1) <- c("Ques. 1")
table.Q1
table.Q2 <- tmp[2]
colnames(table.Q2) <- c("Ques. 2")
table.Q2

#  2. create a contigency table
table.contingency <- data.frame()
for (c in c(1:ncol(tmp)))
{
    count <- c(NA, NA, NA, NA, NA)
    for (i in (1:5))
    {
      count[i] <- sum(tmp[,c]==i)
    }
    table.contingency <- rbind(table.contingency, count)
}

total.cols <- c(NA, NA, NA, NA, NA)
for (c in c(1:ncol(table.contingency)))
{
  total.cols[c] <- sum(table.contingency[,c])
}
table.contingency <- rbind(table.contingency, total.cols)

total.rows <- c(NA, NA, NA, NA)
for (r in c(1:nrow(table.contingency)))
{
  total.rows[r] <- sum(table.contingency[r,])
}
table.contingency <- cbind(table.contingency, total.rows)

colnames(table.contingency) <- c('1', '2', '3', '4', '5', 'Totals')
rownames(table.contingency) <- c('Q1', 'Q2', 'Q3', 'Totals')
table.contingency





## Q 4


