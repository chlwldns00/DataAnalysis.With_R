library(tidyverse)
library(rpart)
library(e1071) # svmLinear
library(caret)
library(kernlab) # svmLinear2
library(rpart.plot)
library(psych)

ds <- read.csv (
  file = "C:/Users/USER/Downloads/wine_dataset.csv",
  header = TRUE
)

#fixed_acidity : (결합산) 주로 타르타르산(tartaric),사과산(malic)으로 이루어져 있고 와인의 산도를 제어
#volatile_acidity : (휘발산) 와인의 향에 연관
#citric_acid : (구연산) 와인의 신선함을 올려주는 역할, 산성화에 연관
#residual_sugar : (잔여 설탕) 와인의 단맛을 올려줌
#chlorides : (염화물) 와인의 짠맛의 원인이며 와인의 신맛을 좌우
#free_sulfur_dioxide, total_sulfur_dioxide : (황 화합물) 원하지 않는 박테리아와 효모를 죽여서 와인을 오래 보관하는 역할
#density : (밀도) 바디의 높고 낮음을 표현하는 와인의 무게감
#pH : (산도) 와인의 신맛의 정도
#sulphates : (계면활성제)

#확인 및 데이터 전처리
str(ds)
head(ds)
sum(is.na(ds))

#데이터 EDA(가술통계)
psych::describe(ds)

ds <- tibble(fixed_acidity = ds$fixed_acidity,
                 volatile_acidity = ds$volatile_acidity,
                 citric_acid = ds$citric_acid,
                 residual_sugar = ds$residual_sugar,
                 chlorides = ds$chlorides,
                 free_sulfur_dioxide = ds$free_sulfur_dioxide,
                 total_sulfur_dioxide = ds$total_sulfur_dioxide,
                 density = ds$density,
                 pH = ds$pH,
                 sulphates = ds$sulphates,
                 alcohol = ds$alcohol,
                 quality = ds$quality,
                 style = as.factor(ds$style))

#상관계수 분석
ds_cor <- ds
ds_cor$style <- as.numeric(ds$style)
cor (ds_cor$fixed_acidity, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$volatile_acidity, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$citric_acid, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$residual_sugar, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$chlorides, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$free_sulfur_dioxide, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$total_sulfur_dioxide, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$density, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$pH, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$sulphates, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$alcohol, ds_cor$style, method = "pearson", use="complete.obs")
cor (ds_cor$quality, ds_cor$style, method = "pearson", use="complete.obs")

#boxplot 분석을 통한 극단값 확인 및 제거
library(dplyr)
boxplot(ds$fixed_acidity)$stats
ds$fixed_acidity <- ifelse(ds$fixed_acidity < 4.5 | ds$fixed_acidity > 9.6, NA, ds$fixed_acidity)
table(is.na(ds$fixed_acidity))
ds <- ds %>% filter(!is.na(fixed_acidity))

boxplot(ds$volatile_acidity)$stats
ds$volatile_acidity <- ifelse(ds$volatile_acidity < 0.08 | ds$volatile_acidity > 0.655, NA, ds$volatile_acidity)
table(is.na(ds$volatile_acidity))
ds <- ds %>% filter(!is.na(volatile_acidity))

boxplot(ds$citric_acid)$stats
ds$citric_acid <- ifelse(ds$citric_acid < 0.04 | ds$citric_acid > 0.6, NA, ds$citric_acid)
table(is.na(ds$citric_acid))
ds <- ds %>% filter(!is.na(citric_acid))

boxplot(ds$residual_sugar)$stats
ds$residual_sugar <- ifelse(ds$residual_sugar < 0.6 | ds$residual_sugar > 17.5, NA, ds$residual_sugar)
table(is.na(ds$residual_sugar))
ds <- ds %>% filter(!is.na(residual_sugar))

boxplot(ds$chlorides)$stats
ds$chlorides <- ifelse(ds$chlorides < 0.009 | ds$chlorides > 0.105, NA, ds$chlorides)
table(is.na(ds$chlorides))
ds <- ds %>% filter(!is.na(chlorides))

boxplot(ds$free_sulfur_dioxide)$stats
ds$free_sulfur_dioxide <- ifelse(ds$free_sulfur_dioxide < 1 | ds$free_sulfur_dioxide > 77, NA, ds$free_sulfur_dioxide)
table(is.na(ds$free_sulfur_dioxide))
ds <- ds %>% filter(!is.na(free_sulfur_dioxide))

boxplot(ds$total_sulfur_dioxide)$stats
ds$total_sulfur_dioxide <- ifelse(ds$total_sulfur_dioxide < 6 | ds$total_sulfur_dioxide > 272, NA, ds$total_sulfur_dioxide)
table(is.na(ds$total_sulfur_dioxide))
ds <- ds %>% filter(!is.na(total_sulfur_dioxide))

boxplot(ds$density)$stats
ds$density <- ifelse(ds$density < 0.98711 | ds$density > 1.00369, NA, ds$density)
table(is.na(ds$density))
ds <- ds %>% filter(!is.na(density))

boxplot(ds$pH)$stats
ds$pH <- ifelse(ds$pH < 2.80 | ds$pH > 3.63, NA, ds$pH)
table(is.na(ds$pH))
ds <- ds %>% filter(!is.na(pH))

boxplot(ds$sulphates)$stats
ds$sulphates <- ifelse(ds$sulphates < 0.22 | ds$sulphates > 0.85, NA, ds$sulphates)
table(is.na(ds$sulphates))
ds <- ds %>% filter(!is.na(sulphates))

boxplot(ds$alcohol)$stats 
ds$alcohol <- ifelse(ds$alcohol < 8.0 | ds$alcohol > 14.0, NA, ds$alcohol)
table(is.na(ds$alcohol))
ds <- ds %>% filter(!is.na(alcohol))

boxplot(ds$quality)$stats 
ds$quality <- ifelse(ds$quality < 4 | ds$quality > 7, NA, ds$quality)
table(is.na(ds$quality))
ds <- ds %>% filter(!is.na(quality))

#6497개의 행 중 1657개 행 삭제. 4840개 행 남음.

# 2개 예측변수 수치형을 명몫형으로 변환.
#fixed_acidity <= 6.6 = Low, 6.6~7.2 사이 = Mid, >= 7.2 = High
# quality 5 = Low, 6 = Mid, 7 = High

ds$quality = ifelse(ds$quality <= 5 , "Low",
             ifelse(ds$quality == 6 , "Mid",
             ifelse(ds$quality >= 7 , "High", ds$quality)))
ds$quality <- as.factor(ds$quality)

ds$fixed_acidity <- ifelse(ds$fixed_acidity <= 6.6 , "Low",
                    ifelse(ds$fixed_acidity > 6.6 & ds$fixed_acidity < 7.2 , "Mid",
                    ifelse(ds$fixed_acidity >= 7.2 , "High", ds$fixed_acidity)))
ds$fixed_acidity <- as.factor(ds$fixed_acidity)

#전처리된 데이터 기술통계
str(ds)
psych::describe(ds)

set.seed(5) # random seed
library(caret)
indexes = createDataPartition(ds$style, p = .7, list = F)
dsTrain = ds[indexes, ]
dsTest = ds[-indexes, ]

# full tree
full_tree<-rpart(style~.,
                 data=dsTrain,
                 control = rpart.control(cp = 0, minsplit = 2, minbucket = 1),
                 method="class"
)
full_tree$cptable
plotcp(full_tree)

rpart.plot(full_tree)

full_tree_predict<-predict(full_tree,dsTest,type="class")
confusionMatrix(dsTest$style,full_tree_predict,mode="everything")
fullTreeAcc <- confusionMatrix(full_tree_predict, dsTest$style)$overall['Accuracy']

#post pruning
post_pruned_tree<-prune(full_tree, cp=full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),"CP"])
rpart.plot(post_pruned_tree)
post_pruned_tree_predict<-predict(post_pruned_tree,dsTest,type="class")
confusionMatrix(dsTest$style,post_pruned_tree_predict,mode="everything")
postTreeAcc <- confusionMatrix(post_pruned_tree_predict, dsTest$style)$overall['Accuracy']

# cp 최적화

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              classProbs = TRUE)

rpartFit1 <- train(style~.,
                   data = dsTrain,
                   method = "rpart",
                   tuneLength = 10,
                   trControl = train.control,
                   metric = "ROC")

rpartFit1
plot(rpartFit1)

#최적 cp 0.004

# 최적 maxdepth
train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              classProbs = TRUE)

system.time(rpartFit2 <- train(style~.,
                               data = dsTrain,
                               method = "rpart2",
                               tuneLength = 10,
                               trControl = train.control,
                               metric = "Accuracy"))

rpartFit2
plot(rpartFit2)

#최적 maxdepth = 6

# minsplit 최적화
# for loop 이용 1부터 100까지 minsplit 을 조절하면서 최대 정확도를 보이는 최대 minsplit 값 선택

# i 와 dataframe을 인자로 받아 인자로 받은i와 minsplit을 i로 하여 나온 정확도를 벡터로 생성하여
# 인자로받은 dataframe에 결합하여 반환해주는 함수 정의 (optMinSplit())
optMinSplit <- function(i, df) {
  fit_o <- rpart(style~.,
                 data = dsTrain,
                 cp = 0.004,
                 maxdepth = 9,
                 minsplit = i
  )
  pred = predict(fit_o, dsTest, type = "class",)
  
  print(i)
  print(confusionMatrix(pred, dsTest$style)[[3]][1])
  
  df <- rbind(df, c(i, confusionMatrix(pred, dsTest$style)[[3]][1]))
  
  return(df)
}
# 빈 dataframe 생성
df_optMinSplit <- data.frame()

# for loop을 이용하여 minsplit 값을 1부터 100까지 넣어가며 결과 확인
for(i in 1:100) {
  df_optMinSplit <- optMinSplit(i, df_optMinSplit)
}

# 그래프로 시각화 (x -> 최적화 파라미터  //  y -> accuracy)
colnames(df_optMinSplit) = c('minsplit', 'Accuracy')

H_optMinSplit <- ggplot(df_optMinSplit, aes(x = minsplit , y = Accuracy)) +
  theme(legend.position="top") + geom_line()

H_optMinSplit

#최적 minsplit = 19
# 이를 적용하여 나무모델 생성
pre_pruned_tree <- rpart(style~.,
               data = dsTrain,
               cp = 0.004,
               maxdepth = 9,
               minsplit = 19
)

rpart.plot(pre_pruned_tree)
pre_pruned_tree_predict<-predict(pre_pruned_tree,dsTest,type="class")
confusionMatrix(dsTest$style,post_pruned_tree_predict,mode="everything")
preTreeAcc <- confusionMatrix(pre_pruned_tree_predict, dsTest$style)$overall['Accuracy']


#신경망 모델에 사용하기 위해 범주형을 수치형으로 변환
dsTrain$fixed_acidity <- as.numeric(dsTrain$fixed_acidity)
dsTrain$quality <- as.numeric(dsTrain$quality)
dsTest$fixed_acidity <- as.numeric(dsTest$fixed_acidity)
dsTest$quality <- as.numeric(dsTest$quality)

preProcValues <- preProcess(ds)
ds <- predict(preProcValues, ds)

#인공신경망 모델
library(nnet)
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(style ~ .,
              data = dsTrain,
              method = 'nnet',
              maxit = 500,
              metric = 'Accuracy',
              # preProcess = c('center', 'scale'), # data normalization
              # We dont need to this, because the data is already scaled
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3,
)

model$finalModel
model$finalModel$convergence 


library(devtools)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model)

# 위 인공신경망 모델에서 각 변수의 중요도 확인
library(NeuralNetTools)
garson(model)
plot(model)

predicted <- predict(model$finalModel, newdata = dsTest, type = "class")
predicted

actual <- dsTest$style
model.confusion.matrix <- table(actual, predicted)
model.confusion.matrix

prediction <- predict(model$finalModel, dsTest, type="class")
prediction <- as.factor(prediction)
confusionMatrix(prediction, dsTest$style)
nnetAcc1 <- confusionMatrix(prediction, dsTest$style)$overall['Accuracy']

# more optimization
tuneGrid = expand.grid(size = 2:15, decay = 5 * 10 ** (-5:-3))
tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 5, 
                         repeats = 2, 
                         returnResamp = 'final')
model2 = train(style ~.,
              data = dsTrain,
              method = 'nnet',
              maxit = 1000,
              metric = 'Accuracy',
              trControl = trControl,
              tuneGrid=tuneGrid
)
model2

model2$finalModel
model2$finalModel$convergence 
plot.nnet(model2)
garson(model2)
plot(model2)

predicted2 <- predict(model2$finalModel, newdata = dsTest, type = "class")
predicted2

actual2 <- dsTest$style
model.confusion.matrix <- table(actual2, predicted2)
model.confusion.matrix

prediction2 <- predict(model2$finalModel, dsTest, type="class")
prediction2 <- as.factor(prediction)
confusionMatrix(prediction2, dsTest$style)
nnetAcc2 <- confusionMatrix(prediction2, dsTest$style)$overall['Accuracy']

library(ROCR)
nn_pred1 <- predict(model, newdata=dsTest, type="raw")
nn_pred1 <- as.numeric(nn_pred1)
nn_pred1 <- ROCR::prediction(nn_pred1, dsTest$style)
nn_model1.roc <- performance(nn_pred1, "tpr", "fpr")   # ROC-chart
plot(nn_model1.roc, colorize=TRUE)
nn_model1.lift <- performance(nn_pred1, "lift", "rpp")  # Lift chart
plot(nn_model1.lift, colorize=TRUE)

nn_pred2 <- predict(model2, newdata=dsTest, type="raw")
nn_pred2 <- as.numeric(nn_pred2)
nn_pred2 <- ROCR::prediction(nn_pred2, dsTest$style)
nn_model2.roc <- performance(nn_pred2, "tpr", "fpr")   # ROC-chart
plot(nn_model2.roc, colorize=TRUE)
nn_model2.lift <- performance(nn_pred2, "lift", "rpp")  # Lift chart
plot(nn_model2.lift, colorize=TRUE)

#여기부터 svm
trControl2 <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model3 <- train(style ~.,
                data = dsTrain,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 10,
                trControl = trControl2
)

model3$finalModel
model3$bestTune
model3
plot(model3)

svm_pred1 <- predict(model3, newdata = dsTest)
confusionMatrix(svm_pred1, dsTest$style)
acc1 <- confusionMatrix(svm_pred1, dsTest$style)$overall['Accuracy']
acc1

tuneGrid2 = expand.grid(cost = seq(15,17,0.1))
tuneGrid2
trControl3 <- trainControl(method = 'repeatedcv', 
                          number = 5, 
                          repeats = 2, 
                          returnResamp = 'final')
model4 <- train(style ~.,
                data = dsTrain,
                method = 'svmLinear2',
                metric = 'Accuracy',
                trControl = trControl3,
                tuneGrid = tuneGrid2
)
model4
model4$finalModel
model4$bestTune
plot(model4)

svm_pred2 <- predict(model4, dsTest)
confusionMatrix(svm_pred2, dsTest$style)
acc2 <- confusionMatrix(svm_pred2, dsTest$style)$overall['Accuracy']
acc2

trControl3 <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model5 <- train(style ~.,
                data = dsTrain,
                method = 'svmRadial',
                metric = 'Accuracy',
                trControl = trControl3,
                #tuneGrid=grid,
                tuneLength = 10
)

model5
model5$finalModel
model5$bestTune
plot(model5)

svm_pred3 <- predict(model5, newdata = dsTest) 
confusionMatrix(svm_pred3, dsTest$style)
acc3 <- confusionMatrix(svm_pred3, dsTest$style)$overall['Accuracy']
acc3

trControl4 <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model6 <- train(style ~.,
                data = dsTrain,
                method = 'svmPoly',
                metric = 'Accuracy',
                trControl = trControl4,
                #tuneGrid=grid,
                tuneLength = 10
)

model6
model6$finalModel
model6$bestTune
plot(model6)

svm_pred4 <- predict(model6, newdata = dsTest) 
confusionMatrix(svm_pred4, dsTest$style)
acc4 <- confusionMatrix(svm_pred4, dsTest$style)$overall['Accuracy']
acc4

result <- tibble(Model = c('ANN',
                            'More Optimized ANN',
                            'SVM Linear', 
                           'SVM Linear w/choice of cost',
                           'SVM Radial',
                           'SVM POly',
                           'Pre-Pruned_Tree',
                           'Full-Tree',
                           'Post-Pruned-Tree'),
                 Accuracy = c(nnetAcc1,
                              nnetAcc2,
                              acc1, 
                              acc2, 
                              acc3,
                              acc4,
                              preTreeAcc,
                              fullTreeAcc,
                              postTreeAcc)
)

result %>% arrange(desc(Accuracy))
