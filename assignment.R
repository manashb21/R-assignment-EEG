#import X data
X = as.matrix(read.csv("X.csv", header = F))
colnames(X) <- c("X1", "X2", "X3", "X4")

Xdf = read.csv("X.csv")
colnames(Xdf) <- c("X1", "X2", "X3", "X4")

#import Y data
Y = as.matrix(read.csv("y.csv", header = F))
colnames(Y) <- c("Y")

#import time data
time = read.csv("time.csv", header = F, skip = 1)
time = as.matrix(rbind(0, time))

#Creating Time series Plot
#create time series objects 
X.ts <- ts(X, start = c(min(time), max(time)),frequency = 1)
Y.ts <- ts(Y, start = c(min(time), max(time)),frequency = 1)

#plotting the graphs
plot(X.ts, main = "Time series plot of X signal", xlab = "Time", ylab = "Input Signal")
plot(Y.ts, main = "Time series plot of Y signal", xlab = "Time", ylab = "Output Signal")

#Creating density plot 
#density plot of X signal
dis = density(X)
par(mfrow = c(1,2))
plot(dis, main = "Density Plot of Input Signal")
#histogram of X signal
hist(X, freq = FALSE, main = "Histogram Plot of Input Signal")

#both density and histogram
par(mfrow = c(1,1))
hist(X, freq = FALSE, main = "Histogram Plot of Input Signal")
lines(dis, lwd = 2, col = "darkblue")
rug(jitter(X))

#density plot of X1 signal
dis_X1 = density(X[,"X1"])
hist(X[,"X1"], freq = FALSE, main = "Histogram Plot of X1")
lines(dis_X1, lwd = 2, col = "orange")
rug(jitter(X[,"X1"]))

#density plot of X2 signal
dis_X2 = density(X[,"X2"])
hist(X[,"X2"], freq = FALSE, main = "Histogram Plot of X2")
lines(dis_X2, lwd = 2, col = "cadetblue")
rug(jitter(X[,"X2"]))

#density plot of X3 signal
dis_X3 = density(X[,"X3"])
hist(X[,"X3"], freq = FALSE, main = "Histogram Plot of X3")
lines(dis_X3, lwd = 2, col = "aquamarine2")
rug(jitter(X[,"X3"]))

#density plot of X4 signal
dis_X4 = density(X[,"X4"])
hist(X[,"X4"], freq = FALSE, main = "Histogram Plot of X4")
lines(dis_X4, lwd = 2, col = "seagreen")
rug(jitter(X[,"X4"]))

#density plot of Y signal
#density plot of X1 signal
dis_Y = density(Y)
hist(Y, freq = FALSE, main = "Histogram Plot of Y")
lines(dis_Y, lwd = 2, col = "purple")
rug(jitter(Y))

#scatter plot between X1 and Y
par(mfrow = c(2,2))
plot(X[,"X1"], Y, main = "Scatter Plot: X1 and Y signal", xlab = "X1 signal",
     ylab = "Output Signal")
abline(lm(X[,'X1'] ~ Y), col = 'red')
#scatter plot between X2 and Y
plot(X[,"X2"], Y, main = "Scatter Plot: X2 and Y signal", xlab = "X2 signal",
     ylab = "Output Signal")
abline(lm(X[,'X2'] ~ Y), col = 'red')
#scatter plot between X3 and Y
plot(X[,"X3"], Y, main = "Scatter Plot: X3 and Y signal", xlab = "X3 signal",
     ylab = "Output Signal")
abline(lm(X[,'X3'] ~ Y), col = 'red')
#scatter plot between X4 and Y
plot(X[,"X4"], Y, main = "Scatter Plot: X4 and Y signal", xlab = "X4 signal",
     ylab = "Output Signal")
abline(lm(X[,'X4'] ~ Y), col = 'red')

#correlation 
#install.packages("corrplot")
#par(mfrow = c(1,1))
#library(corrplot)
#cor_df = cbind(X, Y)
#cor_tab = cor(cor_df)
#corrplot(cor_tab, method = "number", type = "upper")


#TASK 2
#2.1
#estimating model parameters using Least squares theta-hat
Xmodel1 <- cbind((X[,'X4']),(X[,'X1'])^2,(X[,'X1'])^3,(X[,'X2'])^4,(X[,'X1'])^4)
model1thetahat = solve(t(Xmodel1)%*%Xmodel1)%*%t(Xmodel1)%*%Y

Xmodel2 <- cbind((X[,'X4']),(X[,'X1'])^3,(X[,'X3'])^4)
model2thetahat = solve(t(Xmodel2)%*%Xmodel2)%*%t(Xmodel2)%*%Y

Xmodel3 <- cbind((X[,'X3'])^3,(X[,'X3'])^4)
model3thetahat = solve(t(Xmodel3)%*%Xmodel3)%*%t(Xmodel3)%*%Y

Xmodel4 <- cbind((X[,'X2']),(X[,'X1'])^3,(X[,'X3'])^4)
model4thetahat = solve(t(Xmodel4)%*%Xmodel4)%*%t(Xmodel4)%*%Y

Xmodel5 <- cbind((X[,'X4']),(X[,'X1'])^2,(X[,'X1'])^3,(X[,'X3'])^4)
model5thetahat = solve(t(Xmodel5)%*%Xmodel5)%*%t(Xmodel5)%*%Y





