#import X data
X = as.matrix(read.csv("X.csv", header = F))
colnames(X) <- c("X1", "X2", "X3", "X4")

#import Y data
Y = as.matrix(read.csv("y.csv", header = F))
colnames(Y) <- c("Y")

#import time data
time = read.csv("time.csv", header = F, skip = 1)
time = as.matrix(rbind(0, time))

#create time series objects 
X.ts <- ts(X, start = c(min(time), max(time)),frequency = 1)
Y.ts <- ts(Y, start = c(min(time), max(time)),frequency = 1)

#plotting the graphs
plot(X.ts, main = "Time series plot of X signal", xlab = "Time", ylab = "Input Signal")
plot(Y.ts, main = "Time series plot of Y signal", xlab = "Time", ylab = "Output Signal")