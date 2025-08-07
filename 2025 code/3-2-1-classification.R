# 제2절 분류분석 p454
data(iris)
a <- subset(iris, Species == "setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
str(a)
head(a)

b <- glm(Species ~ Sepal.Length, data = a, family = binomial)
summary(b)

# 회귀계수 b와 오즈의 증가량 exp(b)
coef(b)
#######################################################################
# *꽃받침 길이(Sepal.Length)가 1cm 길어질 때,**
# **versicolor일 오즈(확률 비율)는 170배 증가**함.
# 확률 자체가 170배 되는 게 아니라, **오즈**가 170배 증가한다는 점 주의!
exp(coef(b)['Sepal.Length'])
exp(5.14) # 위는 이 수식

# 회귀계수 b와 오즈의 증가량 exp(b)에 대한 신뢰구간
confint(b, parm="Sepal.Length")
# 결과 해석
# log-odds 기준으로, 95% 신뢰수준에서 
# β₁의 참값이 3.42에서 7.42 사이에 있을 가능성이 높다는 의미

exp(confint(b, parm="Sepal.Length"))
# 결과 해석
# Sepal.Length가 1cm 증가할 때, 
# versicolor일 오즈(odds)[승산비]는 약 30.6배 ~ 1661.6배 증가할 것으로 추정

# fitted() 함수로 적합 결과 확인
fitted(b)[c(1:5, 96:100)]

# predict() 함수로 새로운 자료에 대한 예측 수행
# 인자 type="response": 결과가 확률
# 0.165: versicolor(1)일 확률 16.5%
# predict(b, newdata=data.frame(Sepal.Length=c(1, 50, 51, 100)), type="response")
predict(b, newdata=a[c(1, 50, 51, 100),], type="response")

# 인자 type="link": 결과가 로그오즈
predict(b, newdata=a[c(1, 50, 51, 100),], type="link")
log( 0.1657937 / (1-0.1657937) )

# cdplot() 함수로 조건부 분포 시각화
cdplot(Species ~ Sepal.Length, data=a)

# 책처럼 하려면 
# a$Species <- factor(a$Species, levels = c("versicolor", "setosa"))
# cdplot(Species ~ Sepal.Length, data=a)

plot(a$Sepal.Length, a$Species, xlab="Sepal.Length")
x = seq(min(a$Sepal.Length), max(a$Sepal.Length), 0.1)
# 현재, setosa를 1, versicolor를 2로 하려니, 로지스틱 회귀의 확률 함수 + 1
# lines(x, 1+(1/(1+(1/exp(-27.831+5.140*x)))), type='l', col='red')
lines(x, 1 + 1/(1+exp(27.831-5.140*x)), type='l', col='red')

# 종료