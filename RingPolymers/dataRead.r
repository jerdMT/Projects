library(readr)
library(dplyr)
data <- read_table("pos_nc1600N400nhc1600Nh50dT3.0xi1.5step87000000.dat", 
                    col_names = FALSE, col_types = cols(X9 = col_skip()), 
                    skip = 9)
data <- rename(data, ATOM = X1, item=X2, x=X3, y=X4,z=X5,xu=X6,yu=X7,zu=X8)
View(data)

attach(data)

dataOrdered <- data %>% arrange(ATOM)
#arrange data by 1st col
View(dataOrdered)
coord <- data.frame(x,y,z)
veloc <- data.frame(xu,yu,zu)

write.table(dataOrdered,"~/Desktop/Job3/DATASORTED.txt",row.names = F, col.names = F)

