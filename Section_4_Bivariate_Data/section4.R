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

#4


## Q 4


