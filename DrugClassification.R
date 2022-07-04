### --- loading libraries --- #

library(tree)
library(randomForest)
library(gbm)
library(caret)
library(rattle)
library(dplyr)
library(corrplot)

### --- loading the file --- ### 

drugs = read.csv("drug200.csv",header = TRUE)

### --- checking the variables --- ###

ls(drugs)

### --- Dropping missing values --- ###

sum(is.na(drugs)) #check missing values

### --- Creating factor values and then into numeric ones --- ###

drugs$Drug = factor(drugs$Drug) ### except our factor variable 
drugs$Sex = factor(drugs$Sex)
drugs$Cholesterol = factor(drugs$Cholesterol)
drugs$BP = factor(drugs$BP)

### --- Converting the columns into numeric one (preparing for correlation matrix) --- ###
### --- Excluding our factor variable 'Drug' --- ###

drugs$Cholesterol = as.numeric(drugs$Cholesterol)
drugs$Sex = as.numeric(drugs$Sex)
drugs$BP = as.numeric(drugs$BP)

### --- dataset for correlation matrix --- ###

drugcor = select(drugs,"Age", "Sex", "BP","Cholesterol" ,"Na_to_K")

### ---  correlation matrix --- ###

cor0 <- round(cor(drugcor, use = "pairwise.complete.obs"), 2)

### ---  visualizing correlation matrix --- ###

corrplot(cor0, method="pie", type = "upper")

### --- Random split for train and test set --- ###

set.seed(345)
train.index=createDataPartition(drugs$Drug,p=0.7,list=FALSE)
train = drugs[train.index,]
test = drugs[-train.index,]

### writing csv file for train, and test models ###

write.csv(x = train, file = "/Users/georgekalfas/Downloads/Term 2/Group Assignments/ML/ToSend/train.csv", row.names = FALSE)
write.csv(x = test, file = "/Users/georgekalfas/Downloads/Term 2/Group Assignments/ML/ToSend/test.csv", row.names = FALSE)

### --- Caret for trees --- ###

fitcontrol = trainControl(method = "repeatedcv", number = 10,
                          repeats = 5)
set.seed(1)
drugs.rpart = train(train[,-ncol(drugs)], train[,ncol(drugs)],
                    method = "rpart", tuneLength=5,
                    trControl = fitcontrol)
drugs.rpart

### --- Test error --- ### 

pred=predict(drugs.rpart,test[,-ncol(test)]) ##predicting drug for every observation
mean(pred == test[,ncol(test)])

### --- To look at the details of this tree --- ### 

print(drugs.rpart$finalModel)

### --- plot the tree --- ### 

plot(drugs.rpart$finalModel) 
text(drugs.rpart$finalModel,pretty=1)

### --- get fancy trees by rattle --- ### 

fancyRpartPlot(drugs.rpart$finalModel)

### --- Caret for random forest --- ####
fitControl=trainControl(method = "repeatedcv", number = 5,
                        repeats = 3)
set.seed(2) 
rfFit=train(Drug~.,data=train,method="rf",metric="Accuracy",
            trControl=fitControl,tuneLength=4) # Have a look at the model
rfFit
plot(rfFit)
rfFit$finalModel
varImp(rfFit,scale=FALSE)
