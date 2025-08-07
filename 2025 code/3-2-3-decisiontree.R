# 제2절 분류분석 > 3. 의사결정나무 모형 p476

install.packages("rpart")
library(rpart)

c <- rpart(Species ~ ., data=iris)
c
plot(c, compress=T, margin=0.3)
# text(c, cex=1.5)
text(c, cex=1.2)

# 다른 방법
# 분류 모델 예시 (Species 예측)
fit <- rpart(Species ~ ., data = iris, method = "class")
plot(fit)
text(fit, use.n = TRUE)

# [새로운 데이터에 대한 예측 수행 - 앞부분]
head(predict(c, newdata = iris, type = "class"))
# 결과 예시: setosa setosa setosa setosa setosa setosa

# [새로운 데이터에 대한 예측 수행 - 뒷부분]
tail(predict(c, newdata = iris, type = "class"))
# 결과 예시: virginica virginica virginica virginica virginica virginica

###############################################
install.packages("rpart.plot")
library(rpart.plot)
prp(c, type=4, extra=2)


# 여기까지 수업
###########################################
############################################
# 
ls(c)
c$cptable
plotcp(c)

# [1] party 패키지 설치 및 로드
install.packages("party")     # (처음 1회만 설치)
library(party)                # 조건부 추론 트리 함수 포함
data(stagec)
str(stagec)

# [2] 결측값 제거
stagec1 <- subset(stagec, !is.na(g2))         # g2 열의 NA 제거
stagec2 <- subset(stagec1, !is.na(gleason))    # gleason 열의 NA 제거
stagec3 <- subset(stagec2, !is.na(eet))        # eet 열의 NA 제거
str(stagec3)

# [3] 훈련 데이터와 테스트 데이터 분할 (70% : 30%)
set.seed(1234)  # 결과 재현성을 위해 시드 고정
ind <- sample(2, nrow(stagec3), replace = TRUE, prob = c(0.7, 0.3))
ind

trainData <- stagec3[ind == 1, ]  # 훈련용 데이터 (70%)
testData  <- stagec3[ind == 2, ]  # 시험용 데이터 (30%)

# [4] 조건부 추론 트리 모델 생성
tree <- ctree(ploidy ~ ., data = trainData)

# [5] 모델 출력 (구조 확인)
tree
plot(tree)

# [1] 테스트 데이터 예측
testPred <- predict(tree, newdata = testData)

# [2] 예측 결과와 실제 ploidy 비교 (혼동 행렬)
table(testPred, testData$ploidy)

# [3] airquality 데이터의 Ozone 컬럼에서 결측값 제거
airq <- subset(airquality, !is.na(Ozone))

# [4] 연속형 변수(Ozone)에 대한 회귀 트리 모델 생성
airct <- ctree(Ozone ~ ., data = airq)
airct

# [5] 회귀 트리 시각화
plot(airct)

head(predict(airct, data=airq))
predict(airct, data=airq, type="node")
mean((airq$Ozone - predict(airct))^2)

##################

rpart.plot(c, type = 2, extra = 104, cex = 1.2)

