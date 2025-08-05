### p370 ~ 403
### ADsP 데이터분석 과목 > 2. 통계 분석 > 2절 기초 통계 분석

iris
data(iris)
head(iris)
summary(iris)

# 평균, 중앙값, 표준편차, 분산, 1사분위수, 2사분위수, 3사분위수, 최댓값, 최솟값
print(mean(iris$Sepal.Length))
print(median(iris$Sepal.Length))
print(sd(iris$Sepal.Length))
print(var(iris$Sepal.Length))
print(quantile(iris$Sepal.Length, 1/4))
print(quantile(iris$Sepal.Length, 2/4))
print(quantile(iris$Sepal.Length, 3/4))
print(max(iris$Sepal.Length))
print(min(iris$Sepal.Length))

install.packages("MASS")
library(MASS)
data(Animals)
head(Animals)
str(Animals)
summary(Animals)

# 바디와 브레인의 사분위수 출력
print(quantile(Animals$body))
print(quantile(Animals$brain))

###### 2. 회귀 분석
set.seed(2)
x = runif(10, 0, 11)
x
# rnorm(n, mean, sd)
y = 2 + 3*x + rnorm(10, 0, 0.2)
y
dfrm = data.frame(x,y)
print(dfrm)

plot(dfrm$x, dfrm$y)

# linear model, 종속변수 ~ 독립변수
m <- lm(y ~ x, data=dfrm)
m
summary(m)

############
## 다항회귀분석

set.seed(2)
u=runif(10,0,11)
v=runif(10,11,20)
w=runif(10,1,30)

y = 3 + 0.1*u + 2*v - 3*w + rnorm(10, 0, 0.1)

dfrm = data.frame(y,u,v,w)
print(dfrm)
m <- lm(y ~ u+v+w)
m

summary(m)

######
library(MASS)
data(ChickWeight)
str(ChickWeight)
print(head(ChickWeight))

# 1번 닭의 1번 사료에 대해 추출
Chick = ChickWeight[ChickWeight$Diet==1 & ChickWeight$Chick==1,]
print(Chick)

model = lm(weight ~ Time, Chick)
summary(model)

plot(Chick$Time, Chick$weight)

#################################################
## 다항회귀분석, 자기주도학습
# rm(cars)
# data(cars)
print(head(cars))
plot(cars$speed, cars$dist)

cars$speed2 = cars$speed^2
plot(cars$speed2, cars$dist)

model = lm(cars$dist ~ speed+speed2, cars)
summary(model)

##################################################
## Data 생성
x = c(1:9)
y = c(5,3,2,3,4,6,10,12,18)
df = data.frame(x,y)
print(df)

## EDA
plot(df$x, df$y)  # 2차 함수 관계 파악
df$x2 = df$x^2    # x 제곱항 추가

## 1) 제곱항 미포함 회귀식 적합
model1 = lm(y ~ x, df)
summary(model1)
plot(model1)

# 2행2열로 그리기
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# p385
## 2) 제곱항 포함 회귀식 적합
model2 = lm(y ~ x + x2, df)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

plot(7.16667 - 2.91212*df$x + 0.45455*df$x2)
plot(y)

# p388
## (예제) 선형회귀모형 설정 및 후진제거법 통한 변수선택
X1 = c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2 = c(26,29,56,31,52,55,71,31,54,47,40,66,68)
X3 = c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4 = c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y = c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)

df = data.frame(X1,X2,X3,X4,Y)
print(df)
model1 = lm(Y ~ X1+X2+X3+X4, df)
summary(model1)                      # X3 의 유의확률이 가장 높으므로 제거

model2 = lm(Y ~ X1+X2+X4, df)
summary(model2)                      # X4 의 유의확률이 가장 높으므로 제거

model3 = lm(Y ~ X1+X2, df)
summary(model3)

# stepAIC(): 책에 없음, 기본: 추진제거법
stepAIC(lm(Y ~ X1+X2+X3+X4, df))

# 후진제거법, 위와 동일
step(lm(Y ~ X1+X2+X3+X4, df), 
     scope = list(lower = ~1, upper = ~X1+X2+X3+X4), direction = "backward")
step(lm(Y ~ X1+X2+X3+X4, df), direction = "backward") # 위와 동일

# 전진선택법
step(lm(Y ~ 1, df), 
     scope = list(lower = ~1, upper = ~X1+X2+X3+X4), direction = "forward")

# 단계선택법
smodel <- step(lm(Y ~ 1, df), 
          scope = list(lower = ~1, upper = ~X1+X2+X3+X4), direction = "both")
summary(smodel)

# p395, 자기주도학습
library(MASS)
data(hills)
str(hills)
print(head(hills))

model = step(lm(time ~ 1, hills), scope = list(lower = ~1, upper = ~dist+climb), 
             direction = 'both')
summary(model)

## 종료
























































































































