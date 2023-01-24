X = read.csv("X.csv")
colnames(X) <- c("X1", "X2", "X3", "X4")

Y = read.csv("y.csv")
time = read.csv("time.csv")

X.ts <- cbind(X, time)


par(mfrow = c(4,1))


