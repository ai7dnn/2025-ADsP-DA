# 과목4 데이터 분석(제3장 정형 데이터 마이닝, 제3절 군집분석 : 1. 계층적 군집)
rm(list=ls())

install.packages("cluster")
library(cluster)

data(USArrests)
str(USArrests)

d <- dist(USArrests, method="euclidean")
#유클리드 거리 측정 방법 적용
fit <- hclust(d, method="ave")

#평균 연결법으로 군집화
par(mfrow=c(1,2))
#그래프를 2열*1행 공간 확보
plot(fit)
plot(fit, hang=-1)
#plots 영역을 원래대로 그래프 하나만 나오게 환원시킴.
par(mfrow=c(1,1))

#군집 그룹의 수를 6(1~6)개 로 제한
groups <- cutree(fit, k=6)
groups

#덴드로그램을 그린 다음
plot(fit)
rect.hclust(fit, k=6, border="red")
#6개의 그룹을 적색선으로 구분함.

hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k=3, border="red")
#height=50위치, 왼쪽에서 0부터 카운트하여 2번째에서 7번째를 그룹으로 녹색선 박스 표시 
rect.hclust(hca, h=50, which=c(2,7), border=3:4) 


# [예제2] p517~518 빠짐