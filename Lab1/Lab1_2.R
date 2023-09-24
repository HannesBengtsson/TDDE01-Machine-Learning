library("tidyverse")
library("dplyr")
library("kknn")
library("caret")
library("dplyr")
library("visreg")
library("matlib")

#Load data and calculate no of entries
set.seed(12345)
data<-read.csv("parkinsons.csv")
n<-nrow(data)

id<-sample(1:n,floor(n*0.6))
training_data<-data[id,]
test_data<-data[-id,]

scaler<-preProcess(training_data[,5:22])
temp_training<-predict(scaler,training_data[,5:22])
temp_test<-predict(scaler,test_data[5:22])

training_data[,5:22]<-temp_training
test_data[,5:22]<-temp_test

model<-lm(formula= motor_UPDRS ~ 0 +
            Jitter... + Jitter.Abs. + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP +
            Shimmer + Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA +
            NHR + HNR + RPDE + DFA + PPE, data = training_data)
#Summary of the model:
model_summary<-summary(model)
# to visualize the model
#visreg(model) 
# This gives the MSE for training and test data:
training_data_predict<-predict(model, training_data)
test_data_predict<-predict(model, test_data)
training_data_MSE = mean((training_data_predict - training_data$motor_UPDRS)^2)
test_data_MSE = mean((test_data_predict - test_data$motor_UPDRS)^2)

Y<-training_data[,5]
X<-as.matrix(training_data[,7:22])#training_data[,7:22] #<- parameter vector

#log-likelihood function see: page.3 https://www.ma.imperial.ac.uk/~das01/GSACourse/Regression.pdf
loglikelihood<- function(theta,sigma) {
  print(theta)
  n<-length(Y)
  return(-n/2*log(2*pi*sigma^2)-1/(2*sigma^2)*(t(Y-X%*%theta)%*%(Y-X%*%theta)))
  
}

#ridge function
ridge<-function(opt,lambda){
  theta_r<-opt[-1] #veerkar som att denna får ligga före lambda, endimensionell array
  sigma_r<-opt[1]
  return (-loglikelihood(theta_r, sigma_r)+lambda*sum(sapply(theta_r,function(i) i^2)))
}
#ridgeopt function
ridgeopt<-function(lambda){
  optim(par=c(rep(1,17)), fn = ridge, lambda = lambda, method = "BFGS")
  
}
#DF function (fuckad, fixa till)
df<-function(lambda){
  #y=Xtheta_hat=X(t(X)X+lambda*I)^-1*t(X)y=Py. omit y to get P matrix
  lambda_I<-lambda*as.matrix(diag(rep(1,16)))
  p<-X%*%solve(t(X)%*%X+lambda_I)%*%t(X)
  p<-sum(diag(p))
  return (p)
}


#By using function RidgeOpt, compute optimal 𝜽 parameters for 𝜆 = 1, 𝜆 = 100 and 
#𝜆 = 1000. Use the estimated parameters to predict the motor_UPDRS values for training 
#and test data and report the training and test MSE values. 
# Which penalty parameter is most appropriate among the selected ones? 
#Compute and compare the degrees of freedom of these models and make appropriate conclusions.
lambda1<-ridgeopt(1)
lambda100<-ridgeopt(100)
lambda1000<-ridgeopt(1000)

#predicting the motor_UDPRS given the optimal theta values
predict_UDPRS<-function(param,data){
  return(UDPRS_values<-(rowSums(as.matrix(data[,7:22]) %*% as.matrix(diag(param$par[-1])))))
}

pred_training_data_L1<-predict_UDPRS(lambda1,training_data)
pred_training_data_L100<-predict_UDPRS(lambda100,training_data)
pred_training_data_L1000<-predict_UDPRS(lambda1000,training_data)

pred_test_data_L1<-predict_UDPRS(lambda1,test_data)
pred_test_data_L100<-predict_UDPRS(lambda100,test_data)
pred_test_data_L1000<-predict_UDPRS(lambda1000,test_data)

#calculating the MSE for the predicted test and trainingdata

Y_test_data<-test_data[,5]

calc_MSE<-function(Y,Y_hat){
  n<-length(Y)
  return (1/n*sum((Y-Y_hat)^2))
}

MSE_train_L1<-calc_MSE(Y,pred_training_data_L1)
MSE_train_L100<-calc_MSE(Y,pred_training_data_L100)
MSE_train_L1000<-calc_MSE(Y,pred_training_data_L1000)
MSE_test_L1<-calc_MSE(Y_test_data,pred_test_data_L1)
MSE_test_L100<-calc_MSE(Y_test_data,pred_test_data_L100)
MSE_test_L1000<-calc_MSE(Y_test_data,pred_test_data_L1000)

#calculating the degree of freedom for respective lambda
df_1<-df(1)
df_100<-df(100)
df_1000<-df(1000)

print(paste("For 𝜆=1: Training Data MSE:",MSE_train_L1, "Test Data MSE:",MSE_test_L1,"DF:",df_1))
print(paste("For 𝜆=100: Training Data MSE:",MSE_train_L100, "Test Data MSE:",MSE_test_L100,"DF:",df_100))
print(paste("For 𝜆=1000: Training Data MSE:",MSE_train_L1000, "Test Data MSE:",MSE_test_L1000,"DF:",df_1000))
