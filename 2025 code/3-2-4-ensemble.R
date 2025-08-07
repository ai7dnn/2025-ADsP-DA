# 패키지 설치
install.packages("adabag")
install.packages("ggplot2")  # adabag 동작을 위해 필요

# 데이터 불러오기
data(iris)

# adabag 패키지 로드
library(adabag)

# 배깅 모델 생성
iris.bagging <- bagging(Species ~ ., data = iris, mfinal = 10)
iris.bagging

# 10번째 트리 시각화
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

# 9번째 트리 시각화
plot(iris.bagging$trees[[9]])
text(iris.bagging$trees[[9]])

pred <- predict(iris.bagging, newdata=iris)
# pred
tb <- table(pred$class, iris[,5])
tb

###################################################
# 부스팅
# boosting 모델 생성
boo.adabag <- boosting(Species ~ ., data = iris, boos = TRUE, mfinal = 10)
boo.adabag$importance

# 10번째 트리 시각화
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata=iris)
pred
tb <- table(pred$class, iris[,5])
tb

error.rpart <- 1 - ( sum(diag(tb)) / sum(tb) )
error.rpart
#################################### 여기까지 수업

# adaboosting p490
# 패키지 설치 및 로드
install.packages("ada")
library(ada)

# 데이터 불러오기 및 setosa 제거
data(iris)
str(iris)

# 비추천, 다음 문장으로 
# iris[iris$Species != "setosa", ] -> iris
iris <- iris[iris$Species != "setosa", ]
iris
str(iris)
unique(iris$Species)

# 데이터 수 계산
n <- dim(iris)[1]
n

# training data 인덱스 (60%)
trind <- sample(1:n, floor(0.6 * n), FALSE)
trind

# test data 인덱스 (나머지 40%)
teind <- setdiff(1:n, trind)
teind

# 이진 분류를 위한 레이블 조정 (0, 1 값으로 변환)
iris[,5] <- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5]) - 1])
iris
str(iris)


# AdaBoost 모델 학습
gdis <- ada(Species ~ ., data = iris[trind, ], iter = 20, nu = 1, type = "discrete")

# 성능 평가 (test data로 addtest)
gdis <- addtest(gdis, iris[teind, -5], iris[teind, 5])

# 분류기 테스트 결과 시각화
plot(gdis, TRUE, TRUE)
varplot(gdis)
pairs(gdis, iris[trind, -5], maxvar=4)

########################################################
# p493, random forest
# 패키지 설치
install.packages("randomForest")
install.packages("party")       # 최초 1회만 설치

# rpart 패키지 로드 (실제 코드에서는 사용되지 않지만 일부 예제에선 병행 사용)
library(randomForest)
library(rpart)
library(party)                  # 매 세션마다 로드 필요

data(stagec)
stagec3 <- stagec[complete.cases(stagec), ]
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(.7, .3))

trainData <- stagec3[ind==1, ]
testData <- stagec3[ind==2, ]

# 랜덤 포레스트 모델 학습
rf <- randomForest(ploidy ~ ., data = trainData, ntree = 100, proximity = TRUE)

# 예측 결과와 실제 값 비교
table(predict(rf), trainData$ploidy)

# 모델 출력 요약
print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)
rf.pred <- predict(rf, newdata=testData)
table(rf.pred, testData$ploidy)

plot(margin(rf))

######################
set.seed(1234)
cf <- cforest(ploidy ~ ., data=trainData)
cf.pred <- predict(cf, newdata=testData, OOB=TRUE, type="response")
