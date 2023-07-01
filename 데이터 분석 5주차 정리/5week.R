library(dplyr)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)

# read file
ds <- read.csv(file = "C:/Users/82102/Downloads/wine_dataset.csv", header = TRUE)
str(ds)

head(ds)

sum(is.na(ds)) #결측지 제거



ds$style<-factor(ds$style) #red=1, white=2
install.packages("psych")
library("psych")
psych::describe(ds)


# partition of tr, ts set
indexes <- createDataPartition(ds$style, p=.8,list = F)
train<-ds[indexes,]
test<-ds[-indexes,]


# full tree
full_tree<-rpart(style~.,
                 data=train,
                 control = rpart.control(cp = 0, minsplit = 2, minbucket = 1),
                 method="class"
)
full_tree$cptable


rpart.plot(full_tree)

full_tree_predict<-predict(full_tree,test,type="class")

confusionMatrix(test$style,full_tree_predict,mode="everything")


# post pruning 
post_pruned_tree<-prune(full_tree, cp=full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),"CP"])


# compare plot
rpart.plot(post_pruned_tree)
rpart.plot(full_tree)


# prediction
full_tree_predict<-predict(full_tree,test,type="class")
post_pruned_tree_predict<-predict(post_pruned_tree,test,type="class")


# test pruning
confusionMatrix(test$style,full_tree_predict,mode="everything")
confusionMatrix(test$style,post_pruned_tree_predict,mode="everything")


# x = training-size learning curve
training_size_learning_curve <- function(data){
  accuracy_vector <- vector()
  training_size_vector <- vector()
  accuracy_vector_training <- vector()
  
  
  for(i in seq(0.1,0.9,0.1)){
    tr_data <-c()
    pr_data <-c()
    for(j in seq(0,100,1)){
      indexes = createDataPartition(ds$style, p = i, list = F) 
      train <- ds[indexes,]
      test <- ds[-indexes,]
      
      impor_model <- rpart(style~., 
                   data = train,
                   method='class',
                   control = rpart.control(cp = 0.0009,minsplit = 2,minbucket = 1, maxdepth = 5) 
      )
      
      pred_training = predict(impor_model,train,type='class')
      test_true_training = train[train$style==pred_training,]
      pred_accuracy_training = dim(test_true_training)[1]/dim(train)[1]
      
      pred = predict(impor_model, test, type = "class", )
      test_true = test[test$style==pred,]
      pred_accuracy = dim(test_true)[1]/dim(test)[1]
      
      tr_data <-c(tr_data,pred_accuracy_training)
      pr_data <-c(pr_data,pred_accuracy)
    }
    trainning_size = dim(data)[1]*i
    training_size_vector <- c(training_size_vector,trainning_size)
    accuracy_vector_training <- c(accuracy_vector_training,mean(tr_data))
    accuracy_vector <- c(accuracy_vector,mean(pr_data))
  }
  
  plot(training_size_vector,accuracy_vector,ylim=range(0.95,1))
  lines(training_size_vector, accuracy_vector_training, col="red", type="o")
  lines(training_size_vector,accuracy_vector,col = "green", type="o")
  
  
}

training_size_learning_curve(ds)


# x = cp learning curve
cp_learning_curve <- function(data){
  accuracy_vector <- vector()
  cp_size_vector <- vector()
  
  
  for(i in seq(0.0006,0.001,0.00005)){
    pr_data <-c()
    
    for(j in seq(0,100,1)){
      indexes = createDataPartition(ds$style, p = .8, list = F) 
      train <- ds[indexes,]
      test <- ds[-indexes,]
      
      impor_model <- rpart(style~., 
                           data = train,
                           method='class',
                           control = rpart.control(cp = i,minsplit = 2,minbucket = 1, maxdepth = 5) 
      )
      
      pred = predict(impor_model, test, type = "class", )
      test_true = test[test$style==pred,]
      pred_accuracy = dim(test_true)[1]/dim(test)[1]
      
      pr_data <-c(pr_data,pred_accuracy)
      
    }
    
    cp_size = i
    cp_size_vector <- c(cp_size_vector,cp_size)
    accuracy_vector <- c(accuracy_vector,mean(pr_data))
    
  }
  
  plot(cp_size_vector,accuracy_vector,ylim=range(0.98,0.99),type="o",col="red")
}

cp_learning_curve(ds)


# x = minsplit learning curve
minsplit_learning_curve <- function(data){
  accuracy_vector <- vector()
  minsplit_size_vector <- vector()
  
  
  for(i in seq(1,15,1)){
    pr_data <-c()
    
    for(j in seq(0,100,1)){
      indexes = createDataPartition(ds$style, p = .8, list = F) 
      train <- ds[indexes,]
      test <- ds[-indexes,]
      
      impor_model <- rpart(style~., 
                           data = train,
                           method='class',
                           control = rpart.control(cp = 0.0009,minsplit = i,minbucket = 1, maxdepth = 5) 
      )
      
      pred = predict(impor_model, test, type = "class", )
      test_true = test[test$style==pred,]
      pred_accuracy = dim(test_true)[1]/dim(test)[1]
      
      pr_data <-c(pr_data,pred_accuracy)
     
    }
    
    minsplit_size = i
    minsplit_size_vector <- c(minsplit_size_vector,minsplit_size)
    accuracy_vector <- c(accuracy_vector,mean(pr_data))
    
  }
  
  plot(minsplit_size_vector,accuracy_vector,ylim=range(0.95,1),type="o",col="red")
}

minsplit_learning_curve(ds)


# x = minbucket learning curve
minbucket_learning_curve <- function(data){
  accuracy_vector <- vector()
  minbucket_size_vector <- vector()
  
  
  for(i in seq(1,15,1)){
    pr_data <-c()
    
    for(j in seq(0,100,1)){
      indexes = createDataPartition(ds$style, p = .8, list = F) 
      train <- ds[indexes,]
      test <- ds[-indexes,]
      
      impor_model <- rpart(style~., 
                           data = train,
                           method='class',
                           control = rpart.control(cp = 0.0009, minsplit = 2, minbucket = i,maxdepth = 5) 
      )
      
      pred = predict(impor_model, test, type = "class", )
      test_true = test[test$style==pred,]
      pred_accuracy = dim(test_true)[1]/dim(test)[1]
      
      pr_data <-c(pr_data,pred_accuracy)
      
    }
    
    minbucket_size = i
    minbucket_size_vector <- c(minbucket_size_vector,minbucket_size)
    accuracy_vector <- c(accuracy_vector,mean(pr_data))
    
  }
  
  plot(minbucket_size_vector,accuracy_vector,ylim=range(0.95,1),type="o",col="red")
}

minbucket_learning_curve(ds)


# x = maxdepth learning curve
maxdepth_learning_curve <- function(data){
  accuracy_vector <- vector()
  maxdepth_size_vector <- vector()
  
  
  for(i in seq(1,30,1)){
    pr_data <-c()
    
    for(j in seq(0,100,1)){
      indexes = createDataPartition(ds$style, p = .8, list = F) 
      train <- ds[indexes,]
      test <- ds[-indexes,]
      
      impor_model <- rpart(style~., 
                           data = train,
                           method='class',
                           control = rpart.control(cp = 0.0009,minsplit = 2,minbucket = 1, maxdepth = i) 
      )
      
      pred = predict(impor_model, test, type = "class", )
      test_true = test[test$style==pred,]
      pred_accuracy = dim(test_true)[1]/dim(test)[1]
      
      pr_data <-c(pr_data,pred_accuracy)
      
    }
    
    maxdepth_size = i
    maxdepth_size_vector <- c(maxdepth_size_vector,maxdepth_size)
    accuracy_vector <- c(accuracy_vector,mean(pr_data))
    
  }
  
  plot(maxdepth_size_vector,accuracy_vector,ylim=range(0.95,1),type="o",col="red")
}

maxdepth_learning_curve(ds)


# pre pruning
pre_pruned_tree<-rpart(style~.,
                 data=train,
                 control = rpart.control(cp = 0.0009, minsplit = 2, minbucket = 1, maxdepth = 5),
                 method="class"
)

pre_pruned_tree_predict<-predict(pre_pruned_tree,test,type="class")
confusionMatrix(test$style,post_pruned_tree_predict,mode="everything")
rpart.plot(pre_pruned_tree)


# rules
library(dplyr)
rpart.rules(pre_pruned_tree,style = "tallw",cover=T)


rules_ds <- ds%>%filter(total_sulfur_dioxide >= 67 & chlorides < 0.070 & volatile_acidity < 0.83)
rules_ds$style<-factor(rules_ds$style)


rules_indexes <- createDataPartition(rules_ds$style, p=.8,list = F)
rules_train<-rules_ds[rules_indexes,]
rules_test<-rules_ds[-rules_indexes,]


rules_tree<-rpart(style~.,
                 data=rules_train,
                 control = rpart.control(cp = 0.0009, minsplit = 2, minbucket = 1, maxdepth = 5),
                 method="class"
)

rules_tree_predict<-predict(rules_tree,rules_test,type="class")
confusionMatrix(rules_test$style,rules_tree_predict,mode="everything")
