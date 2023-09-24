setwd("~/Desktop/SKOLA/TDDE01/tdde01_labbar/lab2/")
tecator<-read.csv("tecator.csv")
colnames(tecator)

#Divide data
n<-dim(tecator)[1]
set.seed(12345)
id<-sample(1:n, floor(n*0.5))
train<-tecator[id,]
test <- tecator[-id,]

###################################
################ 1 ################
###################################

#Create data set containing Fat & Channels
library(dplyr)
train_fat <- train %>% select(Fat,Channel1:Channel100)
test_fat <- test %>% select(Fat,Channel1:Channel100)
colnames(train_fat)

lm_train <- lm(Fat~., data=train_fat)
summary(lm_train)

pred_train <- predict(lm_train, train_fat)
pred_test <- predict(lm_train, test_fat)

mean(lm_train$residuals^2)
train_MSE = mean((pred_train - train_fat$Fat)^2)
test_MSE = mean((pred_test - test_fat$Fat)^2)
train_MSE #0.005709117
test_MSE #722.4294

#Using MSE to estimate the training- and test data error, it seems like the model is very overfit because the test data MSE is many orders larger than the training data MSE. Further we recognize that model uses all 100 Channel features to predict Fat which is yet another indication that the model is indeed overfitted. Therefore, the quality of the prediction, and thus the quality of the model is considered to be low. 

###################################
################ 2 ################
###################################

#No code needed

###################################
################ 3 ################
###################################

#install.packages("glmnet")
library(glmnet)

#Alpha = 1 is Lasso penalty and Alpha = 0 is ridge penalty
lasso <- glmnet(x=train_fat[,-1],y=train_fat[,1],
                aplha=1, family = "gaussian", lambda=1)

#For lambda = 1 we only keep Channel41 as a feature
#Fat = -12,188797+9,707926*Channel41
coef(lasso) #Non used are equal to 0

x_values<-train_fat[,-1]
y_values<-train_fat[,1]

#Fit and plot the coefficients against log lambda
lambdas <- 10^{seq(from = -1, to = 2, length = 1000)}
lambda_fit <- glmnet(x_values, y_values, alpha = 1, lambda = lambdas)
plot(lambda_fit, xvar = "lambda", main="Part 1.3 LASSO coeff on lambda plot")
#Each line represents a coefficient and for every log λ each coefficient has a certain value. The number of features is derived by checking the number of coefficients with a value distinct from 0. From the plot we conclude that we have three features just before lambda become 1. Analyzing it more in detail we conclude that we get three coefficients when:

#Check for what lambdas we have 3 coefficients
lambda_coeff <- data.frame(lambda_fit$df,lambda_fit$lambda)
index <- which(lambda_fit$df == 3)
three_coeff <- lambda_coeff[index,]
three_coeff
max(three_coeff$lambda_fit.lambda)
min(three_coeff$lambda_fit.lambda)
#0.6931717≤ λ ≤0.9014776 approximately

#Great looking plot av MSE
#cvfit <- cv.glmnet(x=as.matrix(x_values), y=y_values)
#plot(cvfit)

###################################
################ 4 ################
###################################

lambdas_ridge <- 10^{seq(from = -1, to = 2, length = 1000)}
lambda_ridge_fit <- glmnet(x_values, y_values, alpha = 0, lambda = lambdas_ridge)
plot(lambda_ridge_fit, xvar = "lambda", main="Part 1.4 Ridge coeff on lambda plot")
#By comparing the two plots we conclude that using Ridge regression model uses all 100 features for all lambdas. A conclusion is that LASSO can reduce the number of features by eliminating non-significant features completely, but Ridge cannot. 

###################################
################ 5 ################
###################################

#Cross validation with default number of folds
cv_lambda <- cv.glmnet(x=as.matrix(x_values), y=y_values)
plot(cv_lambda, main="Part 1.5 Ridge coeff on lambda plot") #Lambda on MSE
cv_lambda$lambda.min #Optimal λ=0.05744535

#From the plot we conclude that as lambda increases, and thus log λ, so does the CV score (MSE). For the optimal λ = 0.05234206, nine features where chosen. The information in the plot suggests that the optimal λ value doesn’t results in a statistically significantly better prediction than log λ = -4 since log λ = -4 and the optimal lambda result in models that have the same MSE. However, using the optimal lambda result in less features resulting in a simpler model. 

#Get optimal lambda
opt_lambda<-cv_lambda$lambda.min

#Coefficents for opt lambda
opt_coef<-coef(cv_lambda, s = cv_lambda$lambda.min)
opt_coef
opt_lambda_lasso_fit <- glmnet(x_values, y_values, alpha = 1, lambda = opt_lambda)

matrix_x_values<-as.matrix(test_fat[,-1])
optimal_predict <- predict(opt_lambda_lasso_fit, matrix_x_values)

library("reshape2")
library("ggplot2")

df<-data.frame(test$Fat,optimal_predict) 
colnames(df) <- c("Actuals_test","Prediction_optimal_model")

ggplot(data=df,aes(x=Actuals_test,y=Prediction_optimal_model))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  labs(title="Part 1.6")