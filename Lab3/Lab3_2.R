# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

install.packages("kernlab")
library(kernlab)
set.seed(1234567890)

data(spam)
ncol(spam) #Equal to 58
nrow(spam) #Equal to 4601
foo <- sample(nrow(spam)) #Sample reorder the row numbers and saves them into foo
spam <- spam[foo,] #Saves spam with reordered row numbers
spam[,-58]<-scale(spam[,-58]) #Column 58 contains text
tr <- spam[1:3000, ] #3000 for training
va <- spam[3001:3800, ] #800 for validation
trva <- spam[1:3800, ] #Training and validation combined
te <- spam[3801:4601, ]  #801 for test

colnames(spam) #Type is column 58, contains spam or nonspam
?ksvm 

#kernel="rbfdot" is the kernel function used in training 
#and predicting and means Radial Basis kernel "Gaussian"

#kpar contains the list of hyper-parameters to be used with 
#the kernel function, (sigma seems like it's pared with rbfdot)

#C=i meaning c is different for each loop. C stands for the cost of constraints violation
#it's the C-constant for the regularization term in the Largrane formulation

#Scaled=FALSE because we've already scaled the variables

by <- 0.3
err_va <- NULL #The prediction error for the validation 
               #is saved here for each iteration in the loop
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE) #Our model
  mailtype <- predict(filter,va[,-58]) #Prediction on validation data
  t <- table(mailtype,va[,58]) #Confusion matrix
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t)) #Divides number of wrong classifications with total number of cases
}

err_va #Resulting validation error for different Cost of constraints violation

#In the filter0 model we use the C the minimizes the validation error
#which.min(err_va) provides with the row/loop iteration number, this is 
#multiplied with by to give us the C that minimize err_va

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58]) #Prediction for validation data
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0 #This value can also be found in err_va[13]

#The filter1 model is the same as filter0
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58]) #Prediction for test data
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1 #Test error. It's larger than err0 for the validation data

#Training for the filter2 model is done based on the combined 
#training and validation data
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58]) #Prediction for test data
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2 #Slightly better than filter1 if we use tr+va in our training

#Training for the filter3 model is done based on the entire data set
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58]) #Prediction for test data
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3 #Provides much better results than err1 and -2 but is prob. overfitted

############################################
################ QUESTION 1 ################
############################################
#Return filter3 to the user since it is trained on the largest data set

#filter0 and filter1 are trained on the same data set the difference is that they are used to predict on different sets
#filter0 cannot be compared to the others since it predicts on the validation data while the others predict on the test data

#filter1 is not as good as filter2 in predicting, however it uses a smaller data set when training, filter3 uses the entire data set

#filter0/1 1100 support vectors
#filter2 1325 support vectors
#filter3 1561 support vectors

############################################
################ QUESTION 2 ################
############################################

# 2. What is the estimate of the generalization error of the filter returned to the user? 
#err0, err1, err2 or err3? Why?

#err0 0.0675 uses the same model as err1. The size of the prediction data is
#equally large for the two errors but the errors differs a bit
#err1 0.08489388

#err2 0.082397 somewhere in between filter0/1 and filter3

#err3 0.02122347 smallest but uses entire data set giving it a better boundary for this particular data set. However it's probably not as great with other data sets

############################################
################ QUESTION 3 ################
############################################

#Once a SVM has been fitted to the training data, a new point is essentially classified according to the sign
#of a linear combination of the kernel function values between the support vectors and the new point. 
#You are asked to implement this linear combination for filter3. You should make use of the functions 
#alphaindex, coef and b that return the indexes of the support vectors, the linear coefficients for the 
#support vectors, and the negative intercept of the linear combination. See the help file of the kernlab 
#package for more information. You can check if your results are correct by comparing them with the output 
#of the function predict where you set type = "decision". Do so for the first 10 points in the spam dataset. 
#Feel free to use the template provided in the Lab3Block1 2021 SVMs St.R file.

# 3. Implementation of SVM predictions.
length(sv)
length(co)
 
sv<-alphaindex(filter3)[[1]] #Gives us the index for the support vectors
co<-coef(filter3)[[1]] #Coefficients for the support vectors
inte<- - b(filter3)
k<-NULL
kernel <- rbfdot(sigma = 0.05)
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  k2<-NULL
  k2 <- inte
  data <- spam[i,-58]
  for(j in 1:length(sv)){
    svv_data <- spam[sv[j],-58]
    k2<- k2+co[j]*kernel(unlist(data), unlist(svv_data))
  }
  k<-c(k, k2)
}
k
predict(filter3,spam[1:10,-58], type = "decision") #Negative values are nonspam