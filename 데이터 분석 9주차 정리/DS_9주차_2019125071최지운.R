library(rpart)
library(lattice)
library(ggplot2)
library(caret)
library(readr)
library(psych)
require(Epi)
require(pROC)
require(randomForest)
require(mlbench)
library(tidyverse)

ds <- read.csv (
  file = "C:/Users/82102/Downloads/wine_dataset.csv",
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


# 데이터 partition
partition <- createDataPartition(ds$style, p = 0.7, list = F)
train <- ds[partition,]
test <- ds[-partition,]

# class 비율 확인
prop.table(table(ds$style)) * 100
prop.table(table(train$style)) * 100
prop.table(table(test$style)) * 100


########## NAIVE BAYESIAN #########
# Naive Bayesian model 생성
library(e1071)
naive_model <- naiveBayes(style~., data = train)

# Naive Bayesian 예측
naive_pred <- predict(naive_model, test, type = 'class')
head(data.frame(actual = test$style, prediction = naive_pred))

# Naive Bayesian 결과
naive_result <- caret::confusionMatrix(naive_pred, test$style, positive = "white")
naive_result

# Naive Bayesian ROC 곡선
windows()
naive_roc <- Epi::ROC(test = test$style, stat = naive_pred, plot = "ROC")

# Naive Bayesian AUC
naive_roc$AUC



########## PRE-PRUNING #########
# Pre-Pruning 모델 생성
temp_tree <- rpart(style~., data = train, maxdepth = 6, minsplit=19)
pre_tree <- prune(temp_tree, cp = 0.004)

# Pre-Pruning 예측
pre_pred <- predict(pre_tree, test, type = 'class')

# Pre-Pruning 결과
pre_result <- caret::confusionMatrix(pre_pred, test$style, positive = "white")
pre_result

# Pre-Pruning ROC 곡선
windows()
pre_roc <- Epi::ROC(test = test$style, stat = pre_pred, plot = "ROC")
pre_roc
# Pre-Pruning AUC
pre_roc$AUC


########## POST-PRUNING #########
# Post-Pruning 모델 생성
full_tree <- rpart(style~., data = train, 
                   control = rpart.control(cp = 0, minsplit = 2, minbucket = 1), method = "class")
post_model <- prune(full_tree, cp = 0.049)

# Post-Pruning 에측
post_pred <- predict(post_model, test, type = 'class')
head(data.frame(actual = test$style, prediction = post_pred))

# Post-Pruning 결과
post_result <- caret::confusionMatrix(post_pred, test$style, positive = "white")
post_result

# Post-Pruning ROC 곡선
windows()
post_roc <- Epi::ROC(test = test$style, stat = post_pred, plot = "ROC")

# Post-Pruning AUC
post_roc$AUC


########## RANDOM FOREST #########
# Random Forest 모델 생성
rf <- randomForest(style~., data=ds, ntree=100, importance = T)
rf

#변수의 기여도 측정
rf$importance
windows()
varImpPlot(rf)

# Random Forest ROC 곡선
rf_roc <- roc(ds$style, rf$votes[,2])
windows()
plot(rf_roc)
auc(rf_roc)



