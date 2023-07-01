library(dplyr)
library(caret)
library(MASS)

#read file
ds <- read.csv(file = "wine_dataset.csv",
               header = TRUE)

#확인 및 데이터 전처리
str(ds)
head(ds)
sum(is.na(ds))
ds$style<-factor(ds$style) #red=1, white=2


#partition of tr, ts set.
indexes <- createDataPartition(ds$style, p=.8,list = F)
train<-ds[indexes,]
test<-ds[-indexes,]


#최적 예측변수 찾기
library(randomForest)
forest_m <-randomForest(ds$style~.,data=ds)
forest_m$importance

impor_val<-names(forest_m$importance[forest_m$importance>200,]) 
impor_val


# impor_val을 예측변수로 분류분석 실행
library("rpart")
impor_model<-rpart(style~volatile_acidity+chlorides+total_sulfur_dioxide,
                   data=train,
                   cp=0,
                   minsplit=10,
                   minbucket=6,  
                   maxdepth=30, #상관x 
                   method="class"
)
impor_model
#impor_model$control

#모델시각화
library(rpart.plot)
rpart.plot(impor_model)


#예측하기
impor_wine_predict<-predict(impor_model,test,type="class")
head(impor_wine_predict)


#평가
impor_actual<-test$style
impor_actual<-as.factor(impor_actual)
confusionMatrix(impor_actual,impor_wine_predict,mode="everything")




#-------모든변수를 사용한 분류------


#분류분석 실행
model <- rpart(style~.,
               data=train,
               parms=list(split='information'), #information이 정확도 높음
               method = "class"
)
model

#가지치기
pruned_model<-prune(model,cp=0.1)
pruned_model


#모델시각화
rpart.plot(model)
rpart.plot(pruned_model)


#예측하기
wine_predict<-predict(model,test,type="class")
head(wine_predict)

pruned_wine_predict<-predict(pruned_model,test,type="class")
head(pruned_wine_predict)



#평가
actual<-test$style
actual<-as.factor(actual)
confusionMatrix(actual,wine_predict,mode="everything")

confusionMatrix(actual,pruned_wine_predict,mode="everything")

# triain-test 학습곡선
set.seed(1)
acc <- c()
acc2 <- c()
size <- c()
for(i in 3:9){
  indexes <- createDataPartition(ds$style, p=.8,list = F)
  train<-ds[indexes,]
  test<-ds[-indexes,]
  size <- c(size, i/10)
  library("rpart")
  impor_model<-rpart(style~volatile_acidity+chlorides+total_sulfur_dioxide,
                     data=train,
                     cp=0,
                     minsplit=10,
                     minbucket=6,  
                     maxdepth=30, #상관x 
                     method="class"
  )
  
  #예측하기
  impor_wine_predict<-predict(impor_model,test,type="class")
  head(impor_wine_predict)
  
  
  #평가
  impor_actual<-test$style
  impor_actual<-as.factor(impor_actual)
  res <- confusionMatrix(impor_actual,impor_wine_predict,mode="everything")
  print(res[[3]][[1]])
  acc <- c(acc, res[[3]][[1]])
  
  #예측하기
  impor_wine_predict<-predict(impor_model,train,type="class")
  head(impor_wine_predict)
  
  
  #평가
  impor_actual<-train$style
  impor_actual<-as.factor(impor_actual)
  res <- confusionMatrix(impor_actual,impor_wine_predict,mode="everything")
  print(res[[3]][[1]])
  acc2 <- c(acc2, res[[3]][[1]])
}

result_df2 <- data.frame(acc, acc2, size)
result_df2
acc
acc2
size

g<-ggplot(data = result_df2, aes(x = size))
g<-g+geom_line(aes(y=acc), color="red")
g<-g+geom_line(aes(y=acc2), color="blue")
g
