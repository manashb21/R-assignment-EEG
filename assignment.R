#import X data
X = as.matrix(read.csv("X.csv", header = F))
colnames(X) <- c("X1", "X2", "X3", "X4")

#import Y data
Y = as.matrix(read.csv("y.csv", header = F))
colnames(Y) <- c("Y")

#import time data
time = read.csv("time.csv", header = F, skip = 1)
time = as.matrix(rbind(0, time))

