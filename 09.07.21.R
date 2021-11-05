library(HSAUR3)
library(dplyr)
data("roomwidth")
attach(roomwidth)


plot(roomwidth,col = c("red","blue"))

ft <- roomwidth %>% filter(unit == "feet")
mtr <- setdiff(roomwidth,ft) #set operators??

convert=ifelse(roomwidth$unit=="feet", 1, 3.28)

new.width <- roomwidth$width*convert
tapply(roomwidth$width*convert, roomwidth$unit, FUN = summary)
