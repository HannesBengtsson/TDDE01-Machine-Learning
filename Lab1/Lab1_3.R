#Packages
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#Import data
setwd("~/Desktop/SKOLA/TDDE01/tdde01_labbar/lab1/")
data<-read.csv("pima-indians-diabetes.csv", header = FALSE, 
               col.names = c("Times_preg","Glucose_conc",
                             "Diastolic_BP","Triceps_S_T",
                             "TwoHour_SI","BMI","Diabetes_func",
                             "Age","Diabetes"),na.strings=c(""))

###################################
################ 1 ################
###################################

#Changes 0 to No and 1 to Yes
translate<-c("No","Yes")
names(translate)<-c(0,1)
data$Diabetes<-translate[as.character(data$Diabetes)]
data$Diabetes<-as.factor(data$Diabetes)

#Scatterplot showing a Plasma glucose concentration on Age where observations are colored by Diabetes levels.
ggplot(data, aes(x=Age, y=Glucose_conc, color=Diabetes)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes")+ 
  labs(colour="Data type", title="Part 2.1  Actuals plot")


###################################
################ 2 ################
###################################

#Model to predict Diabetes
model <- glm(Diabetes ~ Glucose_conc + Age,family="binomial",data=data)
#set.seed(12345) vet inte om denna tillför?
prob <- predict(model, type="response")
pred <- ifelse(prob>0.5, "Yes", "No")
table(data$Diabetes, pred)
model

#First level = 0 , second = 1
#We predict 1
as.factor(data$Diabetes)

#Missclassification error function
missclass <- function(X,X1){
  n <- length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#In ~1/4 times it predicts the wrong value for the given data
missclass(data$Diabetes, pred)

df <- data.frame(as.factor(pred), data$Glucose_conc, data$Age)

ggplot(df, aes(x=data.Age, y=data.Glucose_conc, color=as.factor.pred.)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes")+ 
  labs(colour="Data type", title="Part 2.2  Prediction plot")

###################################
################ 3 ################
###################################

model
#Equation of the decision boundary between the two classes
#Diabetes = -5.91245 + 0.03564*Glucose_conc + 0.02478*Age
intercept <- model$coefficients[1]
theta_1 <- model$coefficients[2]
theta_2 <- model$coefficients[3]

#y=km+x
#k=-theta_1/theta_0
#m=-intercept/theta_0

slope <- theta_2/-theta_1
intercept_new <- -intercept/theta_1
names(slope) <- "Slope"

#The decisionboundary is a straight line
ggplot(df, aes(x=data.Age, y=data.Glucose_conc, color=as.factor.pred.)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes") +
  geom_abline(intercept = intercept_new, slope = slope, col = "red") #The decision boundary seems to catch the data distribution well

###################################
################ 4 ################
###################################

l_pred <- ifelse(prob>0.2, "Yes", "No") #Lower threshold for Diabetes
h_pred <- ifelse(prob>0.8, "Yes", "No") #Higher threshold

df_l <- data.frame(as.factor(l_pred), data$Glucose_conc, data$Age)
df_h <- data.frame(as.factor(h_pred), data$Glucose_conc, data$Age)

#We get more Yes
ggplot(df_l, aes(x=data.Age, y=data.Glucose_conc, color=as.factor.l_pred.)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes")

#We get more No
ggplot(df_h, aes(x=data.Age, y=data.Glucose_conc, color=as.factor.h_pred.)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes")

###################################
################ 5 ################
###################################
#x1 = Glucose_conc and x2 = Age
z1 <- data$Glucose_conc^4
z2 <- data$Glucose_conc^3*data$Age
z3 <- data$Glucose_conc^2*data$Age^2
z4 <- data$Glucose_conc*data$Age^3
z5 <- data$Age^4
data2 <- cbind(data,z1, z2, z3, z4,z5)

model2 <- glm(Diabetes~ Age + Glucose_conc + z1 + z2 + z3 + z4 + z5
              ,family="binomial",data=data2)
prob2 <- predict(model2, type="response")
pred2 <- ifelse(prob2>0.5, "Yes", "No")

#Makes the decision boundary non-linear, now it has more of a U-shape (+x^2)
ggplot(data2, aes(x=Age, y=Glucose_conc, color=pred2)) + geom_point() +
  scale_color_manual(values = c("black", "orange")) + xlab("Age") + 
  ylab("Plasma glucose concentration") + labs(color="Diabetes")

#Lower missclassification rate than before 
missclass(data$Diabetes, pred2)