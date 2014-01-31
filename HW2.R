install.packages("psych")
install.packages("sm")

library(psych)
library(sm)

setwd("C:/Users/xiaogao/Desktop/coursera/stats")
data <- read.table("Stats1.13.HW.02.txt", header = T)
nrow(data)
names(data)
mean(data$SR)
sd <- sd(data$SR)
var <- sd ^ 2
pre <- subset(data, data[,3] == "pre")
mean(pre$SR)
post <- subset(data, data[,3] == "post")
sd(post$SR)
median(post$SR)

describeBy(post, post$condition)

pre.wm <- subset(pre, pre[,2] == "WM")
post.wm <- subset(post, post[,2] == "WM")
pre.pe <- subset(pre, pre[,2] == "PE")
post.pe <- subset(post, post[,2] == "PE")
pre.ds <- subset(pre, pre[,2] == "DS")
post.ds <- subset(post, post[,2] == "DS")

par(mfrow = c(3,2))
hist(pre.wm$SR)
hist(post.wm$SR)
hist(pre.pe$SR)
hist(post.pe$SR)
hist(pre.ds$SR)
hist(post.ds$SR)

mean(post.wm$SR) - mean(pre.wm$SR)
mean(post.pe$SR) - mean(pre.pe$SR)
mean(post.ds$SR) - mean(pre.ds$SR)
