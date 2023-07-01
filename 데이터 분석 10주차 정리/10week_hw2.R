#install.packages("tidyverse")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages(("patchwork"))
library(patchwork)
#install.packages("psych")
library(psych)
#install.packages("caret")
library(caret)
#install.packages("rpart")
#install.packages("raprt.plot")
library(rpart)
library(rpart.plot)
#install.packages("rattle")
library(rattle)
library(RCurl)

getwd()
setwd("C:/Users/cksgo/Desktop/모든것/수업자료/3학년 1학기/2022_Basic_DataScience - 복사본/10주차/실습")
data <- read.csv (
  file = "concrete_data.csv",
  header = TRUE
)

head(data)
#간단한 전처리,EDA
str(data)
head(data)
sum(is.na(data))
install.packages("lm.beta")
library(lm.beta)
library(psych)
psych::describe(data)

# 설명용 회귀모델
model = lm(Strength ~ Cement+Blast.Furnace.Slag+Fly.Ash+Water+Coarse.Aggregate+Superplasticizer+Fine.Aggregate+Age,data)
lm.beta(model)
summary(lm.beta(model))

# 모델의 설명력은 0.61
# F-statistic p value 가 낮으므로 모델은 유의미
# CoarseAggregate,FineAggregate,Superplasticizer 변수가 p-value값이 높고, 표준회귀계수가 낮으므로  더 나은 예측을 위해 제거 후 다시 summary

new_mod=update(model,.~.-CoarseAggregate-FineAggregate-Superplasticizer) #제거

#투입된 독립변수가 여러개일때는 estimate(비표준화 회귀계수)의 값을 가지고는 어떤변수의 영향력이 더 큰지가 비교 불가능 하나 
#lm.beta(표준화 회귀계수)를 사용하면 상호 비교가 가능하다.
#install.packages("lm.beta")
lm.beta(new_mod)
summary(lm.beta(new_mod))


# 설명모델 생성
# 모든 데이터를 사용해 회귀모델 생성
linearMod <- lm(Strength ~ ., data=data)  
linearMod <- lm(Strength ~ Cement + Fly.Ash,Blast.Furnace.Slag+ Water+Age, data=data)  

print(linearMod)
summary(linearMod) # Multiple R-squared:  0.6155,	Adjusted R-squared:  0.6125 

# 성능 측정 RMSE
rmse <- function(y1,y2){
  sqrt(mean((y1-y2)^2))
}

Predict <- predict(linearMod,data) 

rmse_test <- rmse(Predict,data$Strength)
rmse_test # rmse 10.35361
# rmse : 평균 제곱근 오차 ( 오차의 합을 제곱하여 더한것에 루트 ) 낮을수록 정밀도 높음

linearMod
# 잔차 확인
linearMod$residuals

# 그래프가 정규분포와 비슷한것을 확인 이는 선형모델이 작동하고 있음을 의미
par(mfrow=c(1, 1))
plot(density(linearMod$residuals), 
     main="Density Plot: Residuals", 
     ylab="Frequency", 
     sub=paste("Skewness:", 
               round(e1071::skewness(linearMod$residuals), 2)))
polygon(density(linearMod$residuals), col="red")



# F-test
var.test(Predict,data$Strength) 
# F-test의 p-value = 9.635e-15
# p-value(유의확률)이 9.635e-15 < 0.05 임으로 귀무가설 기각
# 즉 하나 이상의 독립변수가 설명력 증가에 기여 한다고 해석

summary(linearMod)  # model summary
# R-squared : 회귀분석의 성능 평가 척도 중 하나로, 결정력(결정계수)라고도 합니다.
# 이론적으로는 0.6, 실무적으로는 0.4 이상이면 모델이 유의미하다고 봄
# R-squared: 0.6155,	Adjusted R-squared:  0.6125
# 뛰어난 선형 모델

#T-test
# 변수에 대한 유의도 
# 귀무가설: “해당 변수의 회귀계수가 0” 
# 독립변수가 종속변수에 영향을 주지 않음
# p-value는 해당 변수의 회귀계수가 0 일 확률을 나타냄.
# 즉, 해당 변수가 종속변수와 무관할 확률을 나타냄유의 수준 0.05 이하 인지 확인 -> 귀무가설 기각

head(data)

t.test(Predict, data$Cement,var.equal = FALSE)
t.test(Predict, data$Blast.Furnace.Slag,var.equal = FALSE)
t.test(Predict, data$Fly.Ash,var.equal = FALSE)
t.test(Predict, data$Superplasticizer,,var.equal = FALSE)
t.test(Predict, data$Coarse.Aggregate,,var.equal = FALSE)
t.test(Predict, data$Water,var.equal = FALSE)
t.test(Predict, data$Fine.Aggregate,,var.equal = FALSE)
t.test(Predict, data$Age,var.equal = FALSE)

library(olsrr)
# 회귀모델

#ols_regress(Strength ~., data)
# Residual vs Fitted Values Plot
# 비선형성, 동일하지 않은 오차 분산 및 특이치를 탐지하기 위한 그림입니다.
#model <- lm(Strength ~., data)
#ols_plot_resid_fit(model)
# Residual Fit Spread Plot
# 비선형성, 영향력 있는 관측치 및 이상값을 탐지하기 위한 플롯입니다.
#model <- lm(Strength ~., data)
#ols_plot_resid_fit_spread(model)
# Stepwise Regression
# 후보 예측 변수 집합에서 회귀 모형 작성 
# p 값을 기반으로 예측 변수를 입력하고 제거함으로써 
# 더 이상 입력하거나 제거할 변수가 남지 않을 때까지 단계적으로 실행합니다.
# 변수선택
#model <- lm(Strength ~., data)
#k <- ols_step_both_p(model) # hybrid (or stepwise) method
# Plot
#plot(k) 
# Stepwise AIC 회귀 
# Akaike Information Criteria를 기반으로 예측 변수를 제거함으로써, 
# 더 이상 제거할 변수가 남아 있지 않을 때까지 단계적으로 실행합니다.
#model <- lm(Strength ~., data)
#k <- ols_step_backward_aic(model)
#k # No variables have been removed from the model. 지울 예측변수가 없움
#plot(k)
# All Possible Regression (test all the subsets)
#model <- lm(Strength ~., data)
#k <- ols_step_all_possible(model)
#k
#plot(k)
# 최적 부분 집합 회귀 분석 
# 가장 적합한 예측 변수의 부분 집합 선택 
model <- lm(Strength ~., data)
k <- ols_step_best_subset(model)
k
plot(k)


# Stepwise Forward Regression
# Variable Selection
model <- lm(Strength ~., data)
k <- ols_step_forward_p(model) # forward에서 먼저 선택되는 것 중요한변수
k
plot(k)

model <- lm(Strength ~., data)
k <- ols_step_backward_aic(model) # backword 지울 변수가 없다?
k
plot(k)

# Stepwise Regression by p vlaue 
# p value를 통한 변수 선택
k <- ols_step_both_p(model) # 먼저 선택되는 것 중요한 변수
k
plot(k)


relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}    

result=relweights(model,col="blue")
result

library(ggplot2)
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  
}

plotRelWeights(model)



# 예측모델 생성

# By calculating accuracy measures (like min_max accuracy) 
# and error rates (MAPE or MSE), 
# we can find out the prediction accuracy of the model.

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))  
trainingData <- data[trainingRowIndex, ]  # Training set
testData  <- data[-trainingRowIndex, ]   # Testing set
count(trainingData) # 824
count(testData) # 206

# 트레이닝셋으로 데이터 생성
lmMod <- lm(Strength ~ ., data=trainingData)
distPred <- predict(lmMod, testData)  # predict distance
distPred
# 성능 측정
summary (lmMod)
# R-squared : 회귀분석의 성능 평가 척도 중 하나로, 결정력(결정계수)라고도 합니다.
# 이론적으로는 0.6, 실무적으로는 0.4 이상이면 모델이 유의미하다고 봄
# R-squrad 0.6151, djusted R-squared:  0.6113 
# F-statistic: 162.8 on 8 and 815 DF,  p-value: < 2.2e-16

# RMSE 측정
rmse_test <- rmse(distPred,data$Strength)
rmse_test # rmse 21.8252 

# 정확도 및 에러율 측정
actuals_preds <- data.frame(cbind(actuals=testData$Strength, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 77.98% 가 측정됨
correlation_accuracy
head(actuals_preds)
# 실제값과 예측값은 정확도 측정의 form으로써 작동가능

# MinMaxAccuracy and MeanAbsolutePercentageError (MAPE)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
# => 76.03%, 최소 최대 값에 대한 정확도
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape
# => 38.63%, 낮을수록 정확도 높음음
# mean absolute percentage deviation

# k- Fold Cross validation
# K개의 fold를 만들어서 진행하는 교차검증
#install.packages("DAAG")
# Cross valdation 선형회귀
library(DAAG)
windows()
cvResults <- suppressWarnings(
  CVlm(data = data, 
       form.lm=Strength ~ ., 
       m=10, # the number of folds
       dots=FALSE, 
       seed=239, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Strength에 대한 교차검증"));  

CVlm(data = data, 
     form.lm=Strength ~ ., 
     m=10, # the number of folds
)$

attr(cvResults, 'ms')  # => 2856.554 제곱오차 50~60사이

