### p427 ~ 435, 20분 
### ADsP 데이터분석 과목 > 2. 통계 분석 > 4절 시계열 예측

################################
Nile
plot(Nile)
plot(Nile, main = "Nile River Flow (1871–1970)", ylab = "Flow", xlab = "Year")

################################ 
mdeaths
fdeaths
ldeaths
plot(ldeaths)
plot(ldeaths, main = "Log Monthly Deaths from Lung Diseases in UK", ylab = "log(Deaths)")

#################################
ldeaths.decompose <- decompose(ldeaths)
str(ldeaths.decompose)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)

# 계절성을 빼고 그리기 
ldeaths.decompose.adj <- ldeaths - ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)

# seasonal의 핵심만 뽑은 월별 패턴 (12개)
ldeaths.decompose$figure
# seasonal은 매월 같음
ldeaths.decompose$seasonal
########################################################
# 시간에 따라 평균이 일정하지 않은 비정상 시계열 자료: 차분이 필요
Nile.diff1 <- diff(Nile, differences=1)

Nile
Nile.diff1

plot(Nile.diff1)

Nile
Nile.diff2 <- diff(Nile, differences=2)
Nile.diff2
plot(Nile.diff2)

###############
# ACF는 Auto-Correlation Function: 
# 자기 자신과의 전체 상관관계
# lag.max = 20
# 지금 시점과 과거 최대 20시점 전까지의 관계를 계산
acf(Nile.diff2, lag.max=20)
acf(Nile.diff2, lag.max=20, plot=FALSE)

# PACF는 Partial Auto-Correlation Function: 
# 중간 다 빼고 순수한 직접 상관관계
pacf(Nile.diff2, lag.max=20)
pacf(Nile.diff2, lag.max=20, plot=FALSE)

##############
install.packages("forecast")   # 한 번만 설치
library(forecast)              # 매 세션마다 로드

# 자동 모델 
auto.arima(Nile)
Nile.arima <- arima(Nile, order=c(1, 1, 1))
Nile.arima

# 다음 10년 예측
Nile.forecasts <- forecast(Nile.arima, h=10)
Nile.forecasts
plot(Nile.forecasts)

# 추가
models <- list(
  "ARIMA(1,1,1)" = arima(Nile, order = c(1, 1, 1)),
  "ARIMA(2,2,1)" = arima(Nile, order = c(2, 2, 1)),
  "ARIMA(3,2,1)" = arima(Nile, order = c(3, 2, 1))
)

sapply(models, AIC)
sapply(models, BIC)

# 종료