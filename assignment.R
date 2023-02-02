library(matlib)
library(ggplot2)
library(rsample)
#import X data
X = as.matrix(read.csv("X.csv", header = F))
colnames(X) <- c("X1", "X2", "X3", "X4")

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
#par(mfrow = c(1,2))
#plot(dis, main = "Density Plot of Input Signal")
#histogram of X signal
#hist(X, freq = FALSE, main = "Histogram Plot of Input Signal")

#both density and histogram
par(mfrow = c(1,1))
hist(X, freq = FALSE, main = "Density and Histogram Plot of Input Signal")
lines(dis, lwd = 2, col = "tomato2")
rug(jitter(X))

#density plots using GGPLOT
Xdf = read.csv("X.csv")
colnames(Xdf) <- c("X1", "X2", "X3", "X4")
ggplot(Xdf, aes(x=X1))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(Y)), color="#8B3626", linetype="dashed", size=0.6)+
  geom_rug(colour = "#2F4F4F")+
  geom_density(alpha=.1, fill="#FFE1FF")
ggplot(Xdf, aes(x=X2))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+ 
  geom_rug(colour = "#2F4F4F")+
  geom_density(alpha = .1, fill="#FFB90F")
ggplot(Xdf, aes(x=X3))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+ 
  geom_rug(colour = "#2F4F4F")+
  geom_density(alpha = .1, fill="#87CEFF")
ggplot(Xdf, aes(x=X4))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+ 
  geom_rug(colour = "#2F4F4F")+
  geom_density(alpha = .1, fill="#FFBBFF")

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
dis_Y = density(Y)
hist(Y, freq = FALSE, main = "Histogram Plot of Y")
lines(dis_Y, lwd = 2, col = "purple")
rug(jitter(Y))

#density plot of Y using GGPLOT
Ydf = read.csv("Y.csv")
colnames(Ydf) <- c("Y")
ggplot(Ydf, aes(x=Y))+
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(Y)), color="#8B3626", linetype="dashed", size=0.6) +
  geom_rug()+
  geom_density(alpha = .2, fill="#FFE7BA")

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
ones = matrix(1 , length(X)/4,1)
ones
#2.1
#estimating model parameters using Least squares theta-hat
Xmodel1 <- cbind(ones,(X[,'X4']),(X[,'X1'])^2,(X[,'X1'])^3,(X[,'X2'])^4,(X[,'X1'])^4)
model1_thetahat = solve(t(Xmodel1)%*%Xmodel1)%*%t(Xmodel1)%*%Y

Xmodel2 <- cbind(ones,(X[,'X4']),(X[,'X1'])^3,(X[,'X3'])^4)
model2_thetahat = solve(t(Xmodel2)%*%Xmodel2)%*%t(Xmodel2)%*%Y

Xmodel3 <- cbind(ones,(X[,'X3'])^3,(X[,'X3'])^4)
model3_thetahat = solve(t(Xmodel3)%*%Xmodel3)%*%t(Xmodel3)%*%Y

Xmodel4 <- cbind(ones,(X[,'X2']),(X[,'X1'])^3,(X[,'X3'])^4)
model4_thetahat = solve(t(Xmodel4)%*%Xmodel4)%*%t(Xmodel4)%*%Y

Xmodel5 <- cbind(ones,(X[,'X4']),(X[,'X1'])^2,(X[,'X1'])^3,(X[,'X3'])^4)
model5_thetahat = solve(t(Xmodel5)%*%Xmodel5)%*%t(Xmodel5)%*%Y

#Calculating Y-hat and RSS Model 1
Y_hat_m1 = Xmodel1 %*% model1_thetahat
Y_hat_m1
#Calculating RSS
RSS_Model_1=sum((Y-Y_hat_m1)^2)
RSS_Model_1
#Calculating Y-hat and RSS of model 2
Y_hat_m2 = Xmodel2 %*% model2_thetahat
Y_hat_m2
RSS_Model_2=sum((Y-Y_hat_m2)^2)
RSS_Model_2
#Calculating Y-hat and RSS of model 3
Y_hat_m3 = Xmodel3 %*% model3_thetahat
Y_hat_m3
RSS_Model_3=sum((Y-Y_hat_m3)^2)
RSS_Model_3
#Calculating Y-hat and RSS of model 4
Y_hat_m4 = Xmodel4 %*% model4_thetahat
Y_hat_m4
RSS_Model_4=sum((Y-Y_hat_m4)^2)
RSS_Model_4
#Calculating Y-hat and RSS of model 5
Y_hat_m5 = Xmodel5 %*% model5_thetahat
Y_hat_m5
RSS_Model_5=sum((Y-Y_hat_m5)^2)
RSS_Model_5


#calculating variances for all models
N = length(Y)
variance_model1 = RSS_Model_1/(N-1)
variance_model2 = RSS_Model_2/(N-1)
variance_model3 = RSS_Model_3/(N-1)
variance_model4 = RSS_Model_4/(N-1)
variance_model5 = RSS_Model_5/(N-1)

#calculation of loglikelihood
#for model 1
likelihood_1 = -(N/2)*(log(2*pi))-(N/2)*(log(variance_model1))-(1/(2*variance_model1))*RSS_Model_1
likelihood_1
#for model 2
likelihood_2 = -(N/2)*(log(2*pi))-(N/2)*(log(variance_model2))-(1/(2*variance_model2))*RSS_Model_2
likelihood_2
#for model 3
likelihood_3 = -(N/2)*(log(2*pi))-(N/2)*(log(variance_model3))-(1/(2*variance_model3))*RSS_Model_3
likelihood_3
#for model 4
likelihood_4 = -(N/2)*(log(2*pi))-(N/2)*(log(variance_model4))-(1/(2*variance_model4))*RSS_Model_4
likelihood_4
#for model 5
likelihood_5 = -(N/2)*(log(2*pi))-(N/2)*(log(variance_model5))-(1/(2*variance_model5))*RSS_Model_5
likelihood_5

#calculation of AIC (Akaike Information Criterion) and BIC
##Calculating AIC and BIC of model 1
K_model1<-length(model1_thetahat)
K_model1
AIC_model1=2*K_model1-2*likelihood_1
AIC_model1
BIC_model1=K_model1*log(N)-2*likelihood_1
BIC_model1

##Calculating AIC and BIC of model 2
K_model2<-length(model2_thetahat)
K_model2
AIC_model2=2*K_model2-2*likelihood_2
AIC_model2
BIC_model2=K_model2*log(N)-2*likelihood_2
BIC_model2

##Calculating AIC and BIC of model 3
K_model3<-length(model3_thetahat)
K_model3
AIC_model3=2*K_model3-2*likelihood_3
AIC_model3
BIC_model3=K_model3*log(N)-2*likelihood_3
BIC_model3

##Calculating AIC and BIC of model 4
K_model4<-length(model4_thetahat)
K_model4
AIC_model4=2*K_model4-2*likelihood_4
AIC_model4
BIC_model4=K_model4*log(N)-2*likelihood_4
BIC_model4

##Calculating AIC and BIC of model 5
K_model5<-length(model5_thetahat)
K_model5
AIC_model5=2*K_model5-2*likelihood_5
AIC_model5
BIC_model5=K_model5*log(N)-2*likelihood_5
BIC_model5

## Task 2.5
par(mfrow = c(3,2))
## Error of model1
model1_error <- Y-Y_hat_m1
## Plotting the graph QQplot and QQ line of model 1
qqnorm(model1_error, col = "darkcyan",main = "QQ plot of model 1")
qqline(model1_error, col = "red",lwd=1)

model2_error <- Y-Y_hat_m2
qqnorm(model2_error, col = "darkcyan",main = "QQ plot of model 2")
qqline(model2_error, col = "red",lwd=1)

model3_error <- Y-Y_hat_m3
qqnorm(model3_error, col = "darkcyan",main = "QQ plot of model 3")
qqline(model3_error, col = "red",lwd=1)

model4_error <- Y-Y_hat_m4
qqnorm(model4_error, col = "darkcyan",main = "QQ plot of model 4")
qqline(model4_error, col = "red",lwd=1)

model5_error <- Y-Y_hat_m5
qqnorm(model5_error, col = "darkcyan",main = "QQ plot of model 5")
qqline(model5_error, col = "red",lwd=1)

## Chose model 2 based on the values of AIC and BIC, (Lower the better), and low RSS means
## better fit of the regression equation for the data 

### Task 2.7 ## Splitting the data of y into 2 form i.e. Training and testing data set.
set.seed(1353)
split_Y<-initial_split(data = as.data.frame(Y),prop=.7)
# Training Y data split 
Y_training_set<-training(split_Y) 
Y_testing_set<-as.matrix(testing(split_Y)) 
## Testing Y data split 
Y_training_data<-as.matrix(Y_training_set)


## Splitting the data of X into 2 form i.e. Training and testing data set. 
split_X<-initial_split(data = as.data.frame(X),prop=.7) 
## Training X data split 
X_training_set<-training(split_X) 
## Testing X data split 
X_testing_set<-as.matrix(testing(split_X)) 
X_testing_data<-as.matrix(X_testing_set) 
X_training_data<-as.matrix(X_training_set)


### Estimating model parameters using Training set 
training_ones=matrix(1 , length(X_training_set$X1),1) 
#model ->ones,(X[,'X4']),(X[,'X1'])^3,(X[,'X3'])^4
X_training_model<-cbind(training_ones,X_training_set[,"X4"],(X_training_set[,"X1"])^3,(X_training_set[ ,"X3"])^4)
training_thetahat=solve(t(X_training_model) %*% X_training_model) %*% t(X_training_model) %*% Y_training_data

### Model output or model prediction 
#creating X testing model 
testing_ones = matrix(1, length(X_testing_set[,"X1"]), 1)
#selecting X_test_set variables needed as per model 2
X_testing_model = cbind(testing_ones,X_testing_set[,"X4"],(X_testing_set[,"X1"])^3,(X_testing_set[ ,"X3"])^4) 
#computing model output on testing data model
Y_testing_hat = X_testing_model%*% training_thetahat 
Y_testing_hat 
RSS_testing=sum((Y_testing_set-Y_testing_hat)^2) 
RSS_testing

#calculating 95% confidence intervals
##qnorm for normal dist, 0.975 as two sided, 
#sample_sd <- sd(X_testing_data)
#sample_size <- nrow(X_testing_data)
#sample_mean <- mean(X_testing_data)
z <- qnorm(0.975) #as two sided 
#t <- 2.262 for n-1
# ci = sample mean (+/-) z value * (s.d./ root of sample size)
# ci = sample mean (+/-) t(n-1) value * (s.d./ root of sample size)
#c_i_pos <- sample_mean + z * (sample_sd/sqrt(sample_size))
#c_i_neg <- sample_mean - z * (sample_sd/sqrt(sample_size))


error <- Y_testing_set - Y_testing_hat
n_len=length(Y_testing_hat)
C_I_1 = z * sqrt( (error * (1-error) ) / n_len)




#task 3
## Model 2 will be used, parameter are selected and kept constant.
arr_1=0
arr_2=0
f_value=0
s_value=0
model2_thetahat
#values from thetahat
thetabias <- 0.448299550 #chosen parameter
thetaone <- 0.038109255 # chosen parameter
thetatwo <- 0.009827804 # constant value
thetafour <- 0.002092558 # constant value
Epsilon <- RSS_Model_2 * 2 ## fixing value of epsilon
num <- 100 #number of iteration

##Calculating Y-hat for performing rejection ABC
counter <- 0
for (i in 1:num) {
  range1 <- runif(1,-0.448299550,0.448299550) # calculating the range
  range1
  range2 <- runif(1,-0.038109255,0.038109255)
  New_thetahat <- matrix(c(range1,range2,thetatwo,thetafour))
  New_Y_Hat <- Xmodel2 %*% New_thetahat ## New Y hat
  new_RSS <- sum((Y-New_Y_Hat)^2)
  new_RSS
  if (new_RSS > Epsilon){
    arr_1[i] <- range1
    arr_2[i] <- range2
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)
  }
}

par(mfrow = c(1,1))
hist(f_value)
hist(s_value)
###plotting the graph
par(mfrow = c(1,1))
plot(f_value,s_value, col = c("red", "blue"), main = "Joint and Marginal Posterior Distribution")
