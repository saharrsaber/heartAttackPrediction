library(caret)
library(ggplot2)
library(randomForest)
library(tidyverse)
library(e1071)
library(class)
#Importing & splitting data
uci <- read_csv("heart.csv")
set.seed(123)
train.index <- uci$target %>% createDataPartition(p =0.8,list =F)
uci_data <- uci[train.index,]
uci_test <- uci[-train.index,]

#logistic_regression 
lr_model <- glm(target~., data = uci_data, family = "binomial")
prob <- predict(lr_model, newdata = uci_test,type="response")
predictions <- ifelse(prob>0.7, 1, 0)
accuracy_lr <- mean(predictions == uci_test$target)


#random_forest
uci_data$target <- as.factor(uci_data$target)
uci_test$target <- as.factor(uci_test$target)
rf_model <- randomForest(target~.,data=uci_data)
predictions_rf <- predict(rf_model,newdata=uci_test)
accuracy_rf<-mean(predictions_rf==uci_test$target)

#kNN

vector <- rep(0,12)
for(i in 1:12){
  predictions_knn <- knn(train = uci_data, test = uci_test,cl = uci_data$target, k=i)
  vector[i] <- mean(predictions_knn==uci_test$target)
}
vector
predictions_knn <- knn(train = uci_data, test = uci_test,cl = uci_data$target, k=10)
accuracy_knn<-vector[10]

models <- c('Random Forset','kNN','Logistic Regression')
accs <- c(accuracy_rf,vector[10],accuracy_lr)
draw_df <- data.frame(models,accs)

result <- ggplot(draw_df, aes(x=models,y=accs)) +
  geom_col()+xlab("Model Name")+ylab("Model Accuracy")+ggtitle("Accuracy of each model results")
result

table(predictions)
table(predictions_rf)
table(predictions_knn)
AllPredictions <- data.frame(predictions,predictions_rf,predictions_knn)
ggplot(AllPredictions,aes(x=as.factor(predictions)))+
  geom_bar()

ggplot(AllPredictions,aes(x=predictions_knn))+
  geom_bar()

ggplot(AllPredictions,aes(x=predictions_rf))+
  geom_bar()
