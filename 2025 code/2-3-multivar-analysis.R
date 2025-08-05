### p404 ~ 421, 30분 예상
### ADsP 데이터분석 과목 > 2. 통계 분석 > 3절 다변량 분석

install.packages("Hmisc")
library(Hmisc)
data(mtcars)
head(mtcars)

drat <- mtcars$drat
disp <- mtcars$disp
plot(drat, disp)

# 상관계수
cor(drat, disp)
# 상관계수 행렬: 여러 변수의 상관관계
# 상관계수(correlation coefficient)와 유의확률(p-value)를 동시에 계산
rcorr(as.matrix(mtcars), type="pearson")

rst <- rcorr(as.matrix(mtcars), type="pearson")
str(rst)
rst$r
rst$n
rst$P

cov(mtcars)
# 순서를 가지고 상관계수 
rcorr(as.matrix(mtcars), type="spearman")

########################
# p412 다차원 척도볍
data(eurodist)
str(eurodist)
eurodist

# Classical Multidimensional Scaling
loc <- cmdscale(eurodist)
loc
dev.off()
x <- loc[,1]
y <- loc[,2]
plot(x, y, type="n", main="eurodist")
text(x, y, rownames(loc), cex=.8)
abline(v=0, h=0)

# 변환할 차수 지정 가능, 인자 k
cmdscale(eurodist, k=3)

###########################
# p414 주성분 분석
library(datasets)

data("USArrests")
str(USArrests)
head(USArrests)
summary(USArrests)

# 산점도 행렬
pairs(USArrests, main="USArrests data") 
pairs(USArrests, panel=panel.smooth, main="USArrests data") 

# 주성분 분석 (PCA: Principal Component Analysis)
# 변수 간 상관관계를 기반으로 주성분을 추출
# cor = TRUE: 변수 표준화(정규화) 후 PCA 수행 
# 서로 다른 단위를 가진 변수들(Murder, Assault, etc)을 동등하게 처리
fit <- princomp(USArrests, cor=T)
# 각 주성분(Comp.1 ~ Comp.4)의 표준편차, 분산기여도, 누적기여도를 요약 출력
summary(fit)

# 각 주성분에 대한 변수별 계수(로딩값) 출력
# → 주성분이 어떤 변수들의 결합으로 구성됐는지를 보여줌
loadings(fit)
plot(fit, type="lines")
dev.off()
# plot(fit, type="lines")와 동일
screeplot(fit, type="lines")

# 각 행의 콤포넌트 값
fit$scores
biplot(fit)


# 색상 넣어서 biplot 출력
biplot(fit, col = c("gray", "blue"), cex = c(0.7, .8))


# prcomp(): 중심화 + 스케일링된 주성분 score 반환 (more numerically reliable)
prcomp(USArrests, scale = TRUE)
