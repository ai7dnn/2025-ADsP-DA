# 제2절 분류분석 > 2. 신경망 모형 p462
#############
install.packages("nnet")
library(nnet)

nn.iris <- nnet(Species~., data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
summary(nn.iris)

table(iris$Species)
table(iris$Species, predict(nn.iris, type='class'))


### 신경망 시각화 방법 1
## plot.nnet()
install.packages("devtools")
library(devtools)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(nn.iris)

### 신경망 시각화 방법 2
## plotnet()
install.packages("NeuralNetTools")
library(NeuralNetTools)
plotnet(nn.iris) 

### 신경망 시각화 방법 3
## plot()
install.packages("clusterGeneration")
install.packages("scales")
install.packages("reshape")
library(clusterGeneration)
library(scales)
library(reshape)
# plot으로도 그려진다
plot(nn.iris) 

####################
install.packages("neuralnet")
library(neuralnet)

data(infert, package = "datasets")
# case	불임 여부 (1 = 불임, 0 = 정상
str(infert)
head(infert)

net.infert <- neuralnet(case ~ age+parity+induced+spontaneous, 
                        data=infert, hidden=2, err.fct="ce", 
                        linear.output=FALSE, likelihood=TRUE)

plot(net.infert) # p468
names(net.infert)

par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate = "age", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "parity", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "induced", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "spontaneous", min=-2.5, max=5)
par(mfrow=c(1,1))

# 종료












