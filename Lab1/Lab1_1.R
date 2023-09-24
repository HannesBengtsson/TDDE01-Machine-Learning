install.packages("reshape")

library("tidyverse")
library("reshape")
library("dplyr")
library("kknn")

#Load data and calculate no of entries
dataframe<-read.csv("optdigits.csv", header = FALSE)
n<-dim(dataframe)[1]
#Divide into training, validation and test set
#training = 50%, validation = 25% & test = 25%
set.seed(12345)
id<-sample(1:n,floor(n*0.5))
traindata<-dataframe[id,]

id1<-setdiff(1:n, id)
set.seed(12345)
id2<-sample(id1, floor(n*0.25))
validdata<-dataframe[id2,]

id3<-setdiff(id1,id2)
testdata<-dataframe[id3,]


###################################
################ 2 ################
###################################

traindata$V65<-as.factor(traindata$V65)
validdata$V65<-as.factor(validdata$V65)
testdata$V65<-as.factor(testdata$V65)



kknn_traindata<-kknn(V65~. , train=traindata, test=traindata, k=30, kernel="rectangular")
kknn_testdata<-kknn(V65~., train=traindata, test=testdata, k=30, kernel="rectangular")

kknn_traindata_predict<-predict(kknn_traindata)
kknn_testdata_predict<-predict(kknn_testdata)

#confusion matrixes for training and testdata

confusion_matrix_traindata<-table(kknn_traindata_predict,traindata$V65)
confusion_matrix_testdata<-table(kknn_testdata_predict,testdata$V65)

#function for calculating missclassification rates
missclass<-function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#calculating missclassification rates
missclass_traindata<-missclass(kknn_traindata_predict,traindata$V65)
missclass_testdata<-missclass(kknn_testdata_predict,testdata$V65)

###################################
################ 3 ################
###################################

#filters out the cases that should be recognized as an 8 from the testdata
indexes_eights_traindata<-which(traindata$V65 %in% 8)
prob_of_eights_traindata<-kknn_traindata$prob[,9][indexes_eights_traindata]
dataframe_eights<-data.frame("org. index"=indexes_eights_traindata,"probability"=prob_of_eights_traindata,"Predicted value"=kknn_traindata_predict[indexes_eights_traindata])


ordered_eights<-dataframe_eights[order(dataframe_eights$probability,decreasing = TRUE),]

#what we now get is that indexes 129 & 195 of trainingdata is easily recognizable, among many others as the best.
#The three least recognnizable by the algorithm are 520, 431 and 1294

#we create five 8x8 matrixes, H=high rec., L=low rec.
H1<-matrix(as.numeric(traindata[129,-65]),ncol = 8,nrow=8)
H2<-matrix(as.numeric(traindata[195,-65]),ncol = 8,nrow=8)
L1<-matrix(as.numeric(traindata[520,-65]),ncol = 8,nrow=8)
L2<-matrix(as.numeric(traindata[431,-65]),ncol = 8,nrow=8)
L3<-matrix(as.numeric(traindata[1294,-65]),ncol = 8,nrow=8)

#melting the data for ggplot to interpret
melted_H1 <- melt(H1)
melted_H2 <- melt(H2)
melted_L1 <- melt(L1)
melted_L2 <- melt(L2)
melted_L3 <- melt(L3)

#creating heatmaps
H1_plot<-ggplot(melted_H1, aes(x = X1 , y=X2, fill=value)) + labs(x="",y="") + geom_tile() +  scale_fill_gradient(low="white", high="black") + ggtitle("129") + theme(plot.title = element_text(hjust = 0.5))
H2_plot<-ggplot(melted_H2, aes(x = X1 , y=X2, fill=value)) + labs(x="",y="") + geom_tile() +  scale_fill_gradient(low="white", high="black") + ggtitle("195") + theme(plot.title = element_text(hjust = 0.5))
L1_plot<-ggplot(melted_L1, aes(x = X1 , y=X2, fill=value)) + labs(x="",y="") + geom_tile() +  scale_fill_gradient(low="white", high="black") + ggtitle("520") + theme(plot.title = element_text(hjust = 0.5))
L2_plot<-ggplot(melted_L2, aes(x = X1 , y=X2, fill=value)) + labs(x="",y="") + geom_tile() +  scale_fill_gradient(low="white", high="black") + ggtitle("431") + theme(plot.title = element_text(hjust = 0.5))
L3_plot<-ggplot(melted_L3, aes(x = X1 , y=X2, fill=value)) + labs(x="",y="") + geom_tile() +  scale_fill_gradient(low="white", high="black") + ggtitle("1294") + theme(plot.title = element_text(hjust = 0.5))

#######################################
################ 4 & 5 ################
#######################################

TK_vector<-c()
VK_vector<-c()
Validation_CE<-c(rep(0,30))

log_r = function(x){
  -log(x+1e-15)
}

for(i in c(1:30)){
  K_traindata<-kknn(V65~. , train=traindata, test=traindata, k=i, kernel="rectangular")
  K_traindata_predict<-predict(K_traindata)
  K_missclass_traindata<-missclass(K_traindata_predict,traindata$V65)
  TK_vector<-append(TK_vector,K_missclass_traindata,i)
  
  K_validdata<-kknn(V65~. , train=traindata, test=validdata, k=i, kernel="rectangular")
  K_validdata_predict<-predict(K_validdata)
  K_missclass_validdata<-missclass(K_validdata_predict,validdata$V65)
  VK_vector<-append(VK_vector,K_missclass_validdata)
  
  #calculating the cross entropy for validdata
  for (j in 0:9){
    prob_ce <- K_validdata$prob[which(validdata$V65==j),j+1]
    prob_ce_log <- sum(sapply(prob_ce, log_r))
    Validation_CE[i] <- Validation_CE[i] + prob_ce_log
  }
  
}

#Creating the dataframe for the combined plot
df<-data.frame(TK_vector,VK_vector)
colnames(df)<-c("Training data","Validation data")
df$idu<-as.numeric(row.names(df))
df_melted<-melt(df,id="idu")

#combined plot of missclassification rate for validdata and  trainingdata
K_plot<-ggplot(data=df_melted, aes(x=idu, y=value, group=variable, colour=variable)) +
  geom_line()+
  geom_point()+
  labs(y="missclassification rate", x="number of K")

#Plotting the calculated Cross entropy
ce_df<-data.frame(1:30, CE = Validation_CE)
colnames(ce_df)<-c("K","Validation data")
ce_df_melted<-melt(ce_df,id="K")
ce_df_plot<-ggplot(data=ce_df_melted, aes(x=K,y=value)) + 
  geom_line(aes(colour=variable))+
  geom_point(aes(colour=variable))+
  geom_label(aes(x = 12, y = Validation_CE[6]+150, label = "min: K=6, Value=113.768"))

