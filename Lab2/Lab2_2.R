library('dplyr')
library('tidyr')
library('stringr')

#setwd("~/Desktop/SKOLA/TDDE01/tdde01_labbar/lab2/")

###################################
################ 1 ################
###################################

read_data<-read.csv('bank-full.csv')
temp_col<-unlist(str_split(colnames(read_data),'[[:punct:]]'))
data<-separate(data=read_data,col=1,sep=';',into = temp_col)
data<-subset(data,select = -duration)

num.cols <- c('age','campaign','pdays','previous','balance')
data[num.cols] <- lapply(data[num.cols], as.numeric)
fac.cols <- c('job','marital','education','default','housing','loan',
              'contact','day','month','poutcome','y')
data[fac.cols] <- lapply(data[fac.cols], as.factor)

n<-nrow(data)
set.seed(12345)
id<-sample(1:n,floor(n*0.4))

train<-data[id,]
id1<-setdiff(1:n,id)
set.seed(12345)
id2<-sample(id1, floor(n*0.3))
validation <- data[id2,]

id3<-setdiff(id1,id2)
test<-data[id3,]

###################################
################ 2 ################
###################################

library(tree)
n.train<-dim(train)[1]

tree_default <- tree(y~., data=train)
tree_node <- tree(y~., data=train, minsize=7000) #Smallest allowed node size equal to 7000. 
tree_dev <- tree(y~., data=train, mindev=0.0005) #Minimum deviance to 0.0005

plot(tree_default)
title("Default")

plot(tree_node)
title("Min 7000 node size")

plot(tree_dev)
title("Min deviance 0.0005")

summary(tree_default)
summary(tree_node)
summary(tree_dev)

#Missclassification error function
missclass <- function(X,X1){
  n <- length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#MSE tree_default validation
probs_default <- predict(tree_default, validation)
best_index_default <- apply(probs_default, MARGIN=1, FUN = which.max)
pred_default <- levels(validation$y)[best_index_default]
missclass(validation$y, pred_default)

#MSE tree_node validation
probs_node <- predict(tree_node, validation)
best_index_node <- apply(probs_node, MARGIN=1, FUN = which.max)
pred_node <- levels(validation$y)[best_index_node]
missclass(validation$y, pred_node) #BEST

#MSE tree_dev validation
probs_dev <- predict(tree_dev, validation)
best_index_dev <- apply(probs_dev, MARGIN=1, FUN = which.max)
pred_dev <- levels(validation$y)[best_index_dev]
missclass(validation$y, pred_dev)

###################################
################ 3 ################
###################################

#Up to 50 leaves
trainScore <- rep(0,50)
validScore <- rep(0,50)

#Starting at 2 because it only make sense to create a tree with a min of 2 leaves
#The first column in each score vector will be 0.0000
for (i in 2:50) {
  prunedTree <- prune.tree(tree_dev, best=i)
  pred <- predict(prunedTree, newdata = validation, type = 'tree')
  trainScore[i] <- deviance(prunedTree)/nrow(train) 
  validScore[i] <- deviance(pred)/nrow(validation)
}

#Exclude the 0.0000 in the first column
df <- tibble('leaves'=c(2:50),
             'Training data'=trainScore[-1],
             'Valid data'=validScore[-1])
df_pivoted <- pivot_longer(df,cols=2:3,names_to = 'datatype')

#Plot Deviance and No of leaves
library(ggplot2)
ggplot(data = df_pivoted, aes(x=leaves,y=value,colour=datatype))+
  geom_line() + guides(color = guide_legend(title='Data Type'))+
  labs(x='No of leaves', y='Deviance')+
  labs(title="Part 2.3")


#The optimal no of leaves minimize the deviation for the validation data
opt_no_leaves <- df[which.min(df$`Valid data`),]
opt_no_leaves #We get 24 should be 22
tree_optim <- prune.tree(tree_dev, best=opt_no_leaves[1])
pred_optim <- predict(tree_optim, newdata = test, type = 'class')
tree_optim
plot(tree_optim)
text(tree_optim, pretty = 0)
summary(tree_optim)

###################################
################ 4 ################
###################################

#Confusion matrix
confusion_matrix <- table(test$y,pred_optim)
confusion_matrix

#Missclassification rate:
missclass(test$y, pred_optim)


#Precision=TP/(TP+FP)
#Recall=TP/(TP+FN)
TP <- confusion_matrix[2,2]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]

precision <- TP/(TP+FP)
recall <- TP/(TP+FN)

F1_score <- (2*precision*recall)/(precision+recall)
F1_score

#F1 score takes class imbalance into account and since the data is imbalanced, a lot of true no and few true yes, we get this wore F1 score compared to accuracy. Since that data is imbalanced F1-score is preferred in this case. 

###################################
################ 5 ################
###################################

#Perform a decision tree classification of the test data with the following loss matrix
# 0  1    <-  TN  FP
# 5  0        FN  TP

#Type vector because we want the probabilities
prob_pred_optim_tree <- predict(tree_optim, newdata = test, type = 'vector')

prob_pred_optim_tree
predictions <- c()

#Predict no if p(y="no"|x) > 5*p(y="yes"|x) 
for (i in 1:nrow(prob_pred_optim_tree)){
  if(prob_pred_optim_tree[i,1]>prob_pred_optim_tree[i,2]*5){
    predictions <- append(predictions, as.factor('no'))
  }else{
    predictions <- append(predictions, as.factor('yes'))
  }
}

#Confusion matrix for the loss function
conf_loss <- table(test$y,predictions)
conf_loss
missclass_matrix_L <- 1-sum(diag(conf_loss))/length(test$y)
missclass_matrix_L 

TP_L <- conf_loss[2,2]
FP_L <- conf_loss[1,2]
FN_L <- conf_loss[2,1]

precision_L <- TP_L/(TP_L+FP_L)
recall_L <- TP_L/(TP_L+FN_L)

F1_score_L <- (2*precision_L*recall_L)/(precision_L+recall_L)

###################################
################ 6 ################
###################################

pi_boundry <- as.character(seq(from=0.05,to=0.95,by=0.05))
g_linear_reg <- glm(train$y~.,data = train, family="binomial")

test_reg <- as.vector(predict(g_linear_reg,newdata=test,type='response'))
optim_tree <- predict(tree_optim, newdata = test, type = 'vector')

new_tree_pred <- tibble(.rows = nrow(optim_tree))
new_reg_pred <- tibble(.rows = length(test_reg))

calc_scores <- function(Y,Yhat){
  lvls <- sort(union(Y, unlist(Yhat)))
  conf_matrix <- table(factor(Y,levels = lvls), factor(unlist(Yhat),levels = lvls))
  missclassif_rate <-  1-sum(diag(conf_matrix))/length(Y)
  
  TP <- conf_matrix[2,2]
  FP <- conf_matrix[1,2]
  FN <- conf_matrix[2,1]
  TN <- conf_matrix[1,1]
  
  FPR <- FP/(FP+TN)
  TPR <- TP/(TP+FN)
  return(tibble('FPR'=FPR,'TPR'=TPR))
}

tree_scores <- tibble()
reg_scores <- tibble()

for (i in 1:length(pi_boundry)){
  new_tree_pred[[pi_boundry[i]]] <- as.factor(ifelse(optim_tree[,2] > pi_boundry[i], 'yes', 'no'))
  new_reg_pred[[pi_boundry[i]]] <- as.factor(ifelse(test_reg > pi_boundry[i], 'yes', 'no'))
  
  tree_scores <- rbind(tree_scores,calc_scores(Y=test$y, Yhat=new_tree_pred[i]))
  reg_scores <- rbind(reg_scores, calc_scores(Y=test$y, Yhat=new_reg_pred[i]))
}

tree_scores
reg_scores

#For the ROC curve
tree_scores <- cbind(tree_scores, 'var'='tree')
reg_scores <- cbind(reg_scores, 'var'='regression')

combbined_tibble <- tibble(tree_scores) %>% rbind(reg_scores)

ggplot(data = combbined_tibble, aes(x=FPR,y=TPR, color=var))+
  geom_line()+
  scale_color_discrete(name="Type of model",labels=c("Regression","Tree"))+
  labs(title="Part 2.6")
