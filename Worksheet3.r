library(dplyr)
set.seed(123)
#task 1
a1<-dbinom(6,10,0.5)
b1<-dbinom(4,10,0.5)+dbinom(5, size=10, 0.5)
c1<-pbinom(3,10,0.5)
d1<-sum(dbinom(6:10,10,0.5)) #double check
#P(X>=x) DNE P(X>x)
dAlt1 <- pbinom(5,10,0.5,lower.tail = F)
#task 2
a2 = pnorm(-2.15)
b2 = pnorm(2.54,lower.tail = F)
c2 = pnorm(1.96)-pnorm(-1.96)
d2 = qnorm(0.025)
e = qnorm(0.05,lower.tail = F)
f = qnorm(0.01/2)#symmetry

#task 3
data <- rnorm(200)
hist(data,main = "Normal Sample",col = "red")
subdata = sample(data,100,replace = T)
hist(subdata,col = "blue")
mean(subdata)
sd(subdata)
