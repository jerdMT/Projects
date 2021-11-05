#worksheet2
library(dplyr)
library(kableExtra)

library("HSAUR3")
data("Forbes2000")
attach(Forbes2000)


dim(Forbes2000)
colnames(Forbes2000)
head(Forbes2000)
#Return data types
str(Forbes2000)
plot(profits~sales,col = c("blue"), pch = 15, main = "Profit v. Sales", xlab = "Sales", ylab = "Profits")

summary(sales)
summarise(Forbes2000)


hist(profits, main = "Profits", col = "red")
dataR <- Forbes2000[1:15,1:3]
#dataReduced <- Forbes2000 %>% filter(c(1:15)) %>% select(c(1:3))

#mean profits by country
meanpc <- Forbes2000 %>% group_by(country) %>% summarize(Mean = mean(profits))
medianmc <- Forbes2000 %>% group_by(category) %>% summarize((Median = median(marketvalue)))

#begin new chunk

#kable(meanpc)
#kable(medianmc)
