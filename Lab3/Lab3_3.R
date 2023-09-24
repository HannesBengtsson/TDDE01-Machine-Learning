#install.packages("neuralnet")
library(neuralnet)
#setwd("~/Desktop/SKOLA/TDDE01/tdde01_labbar/lab3/")

############################################
################ QUESTION 1 ################
############################################

#Creates a vector of 500 rows, where each value is uniformly distributed between 0 and 10
set.seed(1234567890)
Var <- runif(500, 0, 10)

#Creates a data frame with the Variable and corresponding sinus value
df <- data.frame(Var, Sin=sin(Var))
train <- df[1:25,] # Training
test <- df[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
winit <- runif(10, -1, 1) #10 since we have 10 hidden units that each have a weight

#The number of rows in the hidden vector specifies #of hidden layers
#The actual number specifies the number of hidden units in that layer
nn <- neuralnet(Sin~Var, train, hidden=c(10), startweights = winit) #Start weights is optional

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2, main="Part 3.1 plot")
points(test, col = "blue", cex=1)
points(test[,1],predict(nn,test), col="red", cex=1)
legend("bottomleft", legend=c("Training data", "Test data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)

############################################
################ QUESTION 2 ################
############################################

#Activation functions to use for the act.fct arguement in the nerualnet functions below
linear <- function(x) x
relu <- function(x) ifelse(x>0, x, 0)
softplus <- function(x) log(1 + exp(x))

#h1(x)=x
nn_h1 <- neuralnet(Sin~Var, train, hidden=c(10), startweights = winit, act.fct = linear)
plot(train, cex=2, main="Part 3.2 linear plot")
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h1,test), col="red", cex=1)
legend("bottomleft", legend=c("Training data", "Test data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)

#h2(x)=max{0,x}
nn_h2 <- neuralnet(Sin~Var, train, hidden=c(10), startweights = winit, act.fct = relu)

plot(train, cex=2, main="Part 3.2 relu plot")
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h2,test), col="red", cex=1)
legend("bottomleft", legend=c("Training data", "Test data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)

#h3(x)=ln(1+exp(x))
nn_h3 <- neuralnet(Sin~Var, train, hidden=c(10), startweights = winit,
                   linear.output = TRUE, act.fct = softplus)
plot(train, cex=2, main="Part 3.2 softplus plot")
points(test, col = "blue", cex=1)
points(test[,1],predict(nn_h3,test), col="red", cex=1)
legend("bottomleft", legend=c("Training data", "Test data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)

############################################
################ QUESTION 3 ################
############################################

#Creates a vector of 500 rows, where each value is uniformly distributed between 0 and 50
set.seed(1234567890)
variable <- runif(500, 0, 50)
data <- data.frame(Var=variable, Sin=sin(variable))

#Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2, xlim=c(0,50), ylim=c(-4,1), main="Part 3.3 larger intervall plot")
points(data, col = "blue", cex=1)
points(data[,1],predict(nn,data), col="red", cex=1) #Uses model in Q1
legend("bottomleft", legend=c("Training data", "New data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)

############################################
################ QUESTION 4 ################
############################################
#The predictions converge to ≈-4 as the variable values increases. This is because the first hidden unit of the first layer have a weight that is significantly greater than the other units’ weights. As the variable value gets larger and larger, the impact of this difference increases, and the significance of this unit becomes much larger than that of other hidden units. Therefore, the prediction takes on the output value of the first hidden unit plus the intercept. The value of the first hidden unit is -4.74978 and the intercept is 0.52034, meaning the predictions converge towards -4.74978+0.52034=-4.22944. Further we can see this illustrated in the weights and outputs illustrated in the plot below
plot(nn)
nn$weights

############################################
################ QUESTION 5 ################
############################################
#rev(df)
#Not very good at predicting x from sin(x), most values lays inbetween the two peaks
nn_x <- neuralnet(Var~Sin, df, hidden=c(10), startweights = winit, threshold = 0.1) #Help: Some people get a convergence error in this ques- tion. It can be solved by stopping the training before reaching convergence by setting threshold = 0.1
plot(df, cex=1, main="Part 3.5 predict x from sin(x) plot")
points(train, col = "blue", cex=2)
points(predict(nn_x,train),train[,2], col="red", cex=1)
legend("bottomleft", legend=c("NN training (all)", "Training data", "Prediction"),
       col=c("black", "blue", "red"), pch=1, cex=1,
       box.lty=1)