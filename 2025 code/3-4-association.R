### p552 ~ 555
### ADsP 데이터분석 과목 > 3. 정형 데이터 마이닝 > 4절 연관분석

# 1. 패키지 로드
if (!require("arules")) {
  install.packages("arules")
}
library(arules)
if (!require("arulesViz")) {
  install.packages("arulesViz")
}
library(arulesViz)

# 미국 인구조사국(Census Bureau)의
# 1994년 인구 조사(Public Use Microdata Sample, PUMS) 자료에서 발췌

# 2. 데이터 불러오기 (Adult 데이터셋은 트랜잭션 형태로 제공됨)
data("Adult")
Adult

# 3. 데이터 개요 보기
summary(Adult)
inspect(Adult[1:3])  # 첫 3개 트랜잭션 보기


# 4. 항목 빈도 시각화 (상위 10개)
dev.off()
itemFrequencyPlot(Adult, topN=10, type="absolute",
                  main="Top 10 Items", col="darkgreen")

# 5. 연관 규칙 생성
rules <- apriori(Adult,
                 parameter = list(supp = 0.1, conf = 0.8, target = "rules"))

# 6. 규칙 요약 및 일부 확인
summary(rules)
inspect(head(sort(rules, by = "lift"), 10))

# 7. 규칙 시각화 (scatterplot)
plot(rules, measure = c("support", "confidence"), shading = "lift")

# 8. 규칙 시각화 (grouped 방식)
rules.sorted <- sort(rules, by = "confidence", decreasing = TRUE)
plot(rules.sorted, method = "grouped", control = list(k = 20))

#####################################

# 5. 연관 규칙 생성
rules <- apriori(Adult)
inspect(head(rules))

adult.rules <- apriori(Adult,parameter = list(supp = 0.1, conf = 0.8), 
                       appearance = list(rhs=c('income=small', 'income=large'), default='lhs'), 
                       control = list(verbose=F))
adult.rules.sorted <- sort(adult.rules, by = "lift")
inspect(head(adult.rules.sorted))

# 다음 두 개 동일
plot(adult.rules.sorted)
plot(adult.rules.sorted, method="scatterplot")

# 너무 많아 경고
plot(adult.rules.sorted, method='graph', control=list(type='items', alpha=.5))

rules.top <- head(sort(rules, by = "lift"), 30)
plot(rules.top, method = "graph",
     control = list(layout = "circle"))

#####################################

