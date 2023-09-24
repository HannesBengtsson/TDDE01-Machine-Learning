library(dplyr)
library(tidyr)
library(caret)

############################################
################ QUESTION 1 ################
############################################

#Scale all variables except of ViolentCrimesPerPop
data <- read.csv("communities.csv")
scaler <- data %>% select(-ViolentCrimesPerPop) %>% preProcess()
data.scaled <- predict(scaler,data) %>% sapply(as.numeric)

cov_matrix <- (1/nrow(data.scaled))*t(data.scaled)%*%data.scaled
cov_matrix

eigen_d <- eigen(cov_matrix) #Implement PCA 
eigen_d

#To report how much variance can be explained by the components:
eigen_percent <- (eigen_d$values/sum(eigen_d$values)) %>% cumsum()
numberOfEigen <- eigen_percent[which(eigen_percent<0.95)] %>% length() + 1 #since it will take the last value before hitting 95% variance

#The proportion of variation explained by each of the first two 
first_value_explained <- eigen_d$values[1] #25.02602 %
second_value_explained <- eigen_d$values[2] #16.93153 %

############################################
################ QUESTION 2 ################
############################################

prin_comp <- princomp(data.scaled) #PCA

#The most contributing features are the ones that are furthest away from 0 
plotdata <- tibble('values'=prin_comp$loadings[,1], 'index'=seq(1:length(prin_comp$loadings[,1])))
ggplot(data = plotdata, aes(x=index, y=values))+
  geom_point()

#Najs för att se vad som bidrar mest till modellen som helhet, typ som uppg. 1
#lambda <- prin_comp$sdev^2
#sprintf("%2.3f",lambda/sum(lambda)*100)
#screeplot(prin_comp)

#To check what features that is having a notable contribution to the first component
prin_comp$loadings[,1] %>% abs() %>% sort(decreasing = TRUE)

PC1_PC2 <- tibble('PC1'= prin_comp$scores[,1],'PC2'=prin_comp$scores[,2], 'VCPP'=data$ViolentCrimesPerPop)

ggplot(data = PC1_PC2, aes(x=PC1, y=PC2, color=VCPP))+
  geom_point()+
  scale_color_gradient(low='black',high='red')+
  labs(title="Part 3.2")

############################################
################ QUESTION 3 ################
############################################

data <- read.csv('communities.csv')
set.seed(12345)
n<-dim(data)[1]
id<-sample(1:n,floor(n*0.5))
data.train<-data[id,]

id1<-setdiff(1:n, id)
set.seed(12345)
data.test<-data[id1,]

data.train.scaled <- data.train %>% preProcess() %>% predict(data.train) %>% tibble()
data.test.scaled <- data.train %>% preProcess() %>% predict(data.test) %>% tibble()

model <- lm(data=data.train.scaled, formula = ViolentCrimesPerPop~.)
modelsummary <- summary(model)

train.predict <- predict(model , data.train.scaled)
test.predict <- predict(model , data.test.scaled)

training.data.MSE = mean((train.predict - data.train.scaled$ViolentCrimesPerPop)^2)
test.data.MSE = mean((test.predict - data.test.scaled$ViolentCrimesPerPop)^2)

############################################
################ QUESTION 4 ################
############################################

#Hint 1: don’t store parameters from each iteration (otherwise it will take a lot of memory), instead compute and store test errors directly. 
#Hint 2: discard some amount of initial iterations, like 500, in your plot to make the dependences visible.

train.MSE <- list()
test.MSE <- list()
k <- 0

train <- as.matrix(data.train.scaled[,1:ncol(data.train.scaled)-1])
test <- as.matrix(data.test.scaled[,1:ncol(data.test.scaled)-1])
train_true <- as.matrix(data.train.scaled[,ncol(data.train.scaled)])
test_true <- as.matrix(data.test.scaled[,ncol(data.test.scaled)])

#Cost function for linear regression without intercept on the training data set
cost_function <- function(theta){
  theta <- as.matrix(theta)
  cost_train <- mean((train %*% theta - train_true)^2)
  cost_test <- mean((test %*% theta - test_true)^2)
  
  .GlobalEnv$k <- .GlobalEnv$k+1
  .GlobalEnv$train.MSE[[k]] <- cost_train
  .GlobalEnv$test.MSE[[k]] <- cost_test
  return(cost_train)
}

#Optimize this cost with starting point θ^0=0
optim_theta <- optim(par=c(rep(0,ncol(train))), fn=cost_function, method = 'BFGS')
combined_tibble <- tibble('train.MSE'=as.numeric(train.MSE),'test.MSE'=as.numeric(test.MSE),'k'=seq(from=1,to=length(train.MSE),by=1))

ggplot(data = combined_tibble[500:length(train.MSE),])+
  geom_line(aes(y=train.MSE,x=k,color='Training data'))+
  geom_line(aes(y=test.MSE,x=k,color='Test data'))+
  coord_cartesian(ylim = c(0.2,0.5),xlim = c(0,10000)) +
  ylab("MSE") + xlab("No of iterations")+
  labs(title="Part 3.4")

optim_valid <- combined_tibble[which.min(combined_tibble$test.MSE),]