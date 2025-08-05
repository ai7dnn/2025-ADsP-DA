####### 패키지 reshape ############################
rm(list = ls())

# reshape 패키지는 r에 내장된 명령문과 다름.
if (!require(reshape)) {
  install.packages("reshape")
}

library(reshape) # 메모리 로드 

data("airquality") # 뉴욕의 대기질 데이터
head(airquality, 10)
str(airquality)

# 데이터프레임에서는 names()와 colnames() 동일
names(airquality) 
colnames(airquality)
rownames(airquality)
tolower(names(airquality))

# 열이름, 소문자로 일괄 변환
colnames(airquality) = tolower(names(airquality))
names(airquality)
colnames(airquality)
head(airquality, 10)

## melt() cast()
# melt() : 넓은 자료구조를 길게(wide to long) 재구조화 해주는 함수
#         id를 기준으로 나머지 각 변수를 variable이란 데이터로 만듦
#         id로 지정한 월, 날짜별로 
#         나머지 열을 모두 variable, value에 값을 늘여놓는 방식

# 결측값 NA(Not Available)도 처리
aqm = melt(airquality, id = c("month", "day"))
head(aqm, 5)
tail(aqm, 5)
aqm[!complete.cases(aqm), ] # NA가 있는 행 출력

install.packages("dplyr")
library(dplyr)
sample_n(airquality, 5)
sample_n(aqm, 5)

## melt()
aqm = melt(airquality, id=c("month", "day"), na.rm = TRUE)
head(aqm)
sample_n(aqm, 5)
# NA가 있는 행 출력, 없음
aqm[!complete.cases(aqm), ] 

## cast(): (long -> wide 형식으로)
# aqm 대상으로 day를 y 축, month를 x축 
# 그리고 각 variable들의 값을 안에 넣어서 보여주는 형식
# 반환형은 3차원 배열. [일, 월, 변수]
# 변수 : ozone solar.r wind temp 
a <- cast(aqm, day ~ month ~ variable) # 결과는 3차원 배열
a 

class(a)   # 배열 유형
str(a)
# typeof(a)  # double
# mode(a)    # numeric

a[1, 1, 1] # 1일, 5월, ozone
a[1, 1, 'ozone'] # 1일, 5월, ozone
a[1:5, , 'ozone'] # *, *, ozone
a[1:3, , ] # *, *, *

a[1, 1, 2] # 1일, 5월, solar.r
a[1, 1, 'solar.r'] # 1일, 5월, solar.r

# 1일, 5월, solar.r: solar.r의 1행에서 5행
a[1:5, , 'solar.r'] 

# 월별 각 변수(오존, 태양복사, 바람, 온도)들의 평균값 산출 
b <- cast(aqm, month ~ variable, mean) 
head(b)
b <- cast(aqm, month ~ variable, fun.aggregate=mean)
b 

# month, day: 열 구성, 나머지 var을 열로 구성
head(cast(aqm, month + day ~ variable))
# 일자별, 지정한 함수가 없어서 length 
head(cast(aqm, day ~ variable))
head(cast(aqm, day ~ variable, max))

# month 별로 모든 변수에 대해 평균을 구하고
# 이어서 | 를 사용해 산출물을 분리해서 리스트로 반환
aqm = melt(airquality, id=c("month", "day"), na.rm = TRUE)
sample_n(aqm, 5)
# 데이터 재구조화 + 통계 요약(mean)**을 수행
# cast() 구문은 다음과 같은 구조를 가진다:
#   month ~ . | variable:
#   행(row)은 month로 구분
# .은 요약 대상(여기선 전체를 의미)
# | variable: 변수(Ozone, Temp 등)를 기준으로 나눔
# mean은 각 변수에 대해 월별 평균값을 계산하게 만든다.
# → 결과는 **변수별로 분할된 테이블(리스트 형태)**이 된다. 예:
c <- cast(aqm, month ~ . | variable, mean) 
str(c)

c[[1]]
c[1]

# 각 변수의 평균을 참조, 리스트 참조 방법
c[['ozone']]
c[['solar.r']]
c[['wind']]
c[['temp']]
c['temp'] # list

############# 일자로 모든 변수의 평균
# 기준을 day로 변경 (일별로 변수 평균 보기)
cast(aqm, day ~ . | variable, mean)

############# 월별로 모든 변수의 평균, 값은 의미 없음
c2 <- cast(aqm, month ~ ., mean) 
c2 
##################

# 가장 밑, 맨 오른쪽에 소합계나 소평균 산출 
# 이 경우는 평균
d <- cast(aqm, month ~ variable, mean, 
          margins = c("grand_row", "grand_col"))
head(d) 

d2 <- cast(aqm, month ~ variable, mean, 
           margins = TRUE)
head(d2) 

head(airquality)
# 일, 월 별로 subset을 이용해 ozone에 대해서만 평균(일 월의 값) 처리.
e <- cast(aqm, day ~ month, mean, subset = variable=="ozone")
head(e)

# 다음도 위와 동일
e1 <- cast(aqm, day ~ month, subset = variable=="ozone")
head(e1)

# 일, 월 별로 subset을 이용해 solar.r에 대해서만 평균 처리
# 실제는 평균이 아니고 하나의 데이터임
e2 <- cast(aqm, day ~ month, mean, subset = variable=="solar.r")
head(e2)

e3 <- cast(aqm, day ~ month, mean, subset = variable=="wind")
head(e3)

f <- cast(aqm, day ~ variable, range)
f

# the end