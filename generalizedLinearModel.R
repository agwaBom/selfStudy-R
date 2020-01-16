#일반화 선형모델이 필요한 이유
#1 - 선형을 벗어나는 그래프가 나올때
#설명변수 - x , 반응변수 - y

#할인율에 따른 이익
muffler = data.frame(discount = c(2.0, 4.0, 6.0, 8.0, 10.0),
                     profit = c(0, 0, 0, 1, 1))

#cex - number indicating the amount by which plotting text and symbols should be scaled relative to the default. 1=default, 1.5 is 50% larger, 0.5 is 50% smaller, etc.
plot(muffler, pch = 20, cex = 2, xlim = c(0, 12))

#lm은 설명변수와 반응변수 사이의 선형 모델을 만든다.
#use muffler data frame to draw y(반응변수) ~ x(설명변수 == 원인) linear graph
m = lm(formula = profit ~ discount, data = muffler) 

coef(m) #coefficient
fitted(m) #return fitted value to each dot via linear model m
residuals(m) #fitted - actual data = residuals(잔차)
deviance(m) #잔차의 제곱합 - 음수값을 없얘어 순수한 차이를 얻기 위해서 deviance를 사용

#부적절한 모델 적용
plot(muffler, pch = 20, cex = 2, xlim = c(0, 12))
abline(m)

newd = data.frame(discount = c(1, 5, 12, 20, 30))
p = predict(m, newd)
print(p)

plot(muffler, pch = 20, xlim = c(0, 32), ylim = c(-0.5, 4.2))
abline(m)

res = data.frame(discount = newd, profit = p)
points(res, pch = 20, cex = 2, col = 'red')
#안내표(명각)
legend("bottomright",
       legend = c("train data", "new data"), 
       pch = c(20, 20), 
       cex = 2,
       col = c("black", "red"),
       bg = "gray")

muffler = data.frame(discount = c(2.0, 4.0, 6.0, 8.0, 10.0),
                     profit = c(0, 0, 0, 1, 1))
#glm 함수 적용
#family는 종속변수의 분표에 따라 다르게 설정
#정규분포 - gaussian
#이항분포 - binomial
#포아송 분포 - poisson
#역정규분포 - inverse.gaussian
#감마분포 - gamma
#유사가능도 모형 - quasi(응답분포가 확실하지 않을때 사용)
g = glm(profit ~ discount, data = muffler, family = binomial)
g
coef(g)
fitted(g)
residuals(g)
deviance(g)
abline(g, col = 'red', lwd = 2) #lwd - line width relative to default

newd = data.frame(discount = c(1, 5, 12, 20, 30))

#https://stackoverflow.com/questions/47486589/what-is-the-difference-between-type-response-terms-and-link-in-predict-f
#g의 일반화 선형 모델을 이용하여 newd에 대한 profit값을 추측
#f(y) = ax + bx2 ...
p = predict(g, newd, type = 'response') #to return response - "natural" scale위에 있도록 출력 
p = predict(g, newd, type = 'link') #f(y)의 값을 출력
p = predict(g, newd, type = 'terms') #각 설명변수에 대한 결과치를 matrix로 출력 => f(y) + discount(coef) 
#-137.82190(f(y)) + 22.99(discount coef) = terms에서의 값. 
plot(muffler, pch = 20, xlim = c(0, 30), ylim = c(-150, 500))
print(p)

plot(muffler, pch = 20, cex = 2, xlim = c(0, 32))
abline(g, col = "blue", lwd = 2)
#예측치를 profit에, newd를 discount에 넣은 데이터프레임을 만든 후, res에 입력
res = data.frame(discount = newd, profit = p)
points(res, pch = 20, cex = 2, col = 'red')
legend("bottomright", 
       legend = c("train data", "new data"), 
       pch = c(20, 20),
       cex = 2,
       col = c("black", "red"),
       bg = "gray") #background

###############################
library(ggplot2)

plot(muffler, pch = 20, cex = 2, xlim = c(0, 32))
abline(g, col = 'blue', lwd = 2)
ggplot(data = g, aes(x = discount, y = profit)) + geom_point()
res = data.frame(discount = newd, profit = p)
points(res, pch = 20, cex = 2, col = 'red')
legend("topleft",#x = -1, y = 1.1, #can just write "center" 
       legend = c("train data", "new data"),
       pch = c(20, 20),
       cex = 1,
       col = c("black", "red"),
       bg = "gray")
###############################

#haberman survival data
#age, op_year, no_nodes(설명 변수)로 survival(반응 변수)예측모델 만들기
haberman = read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data", header = F)
names(haberman) = c('age', 'op_year', 'no_nodes', 'survival')
str(haberman) #survival 값이 1, 2이기 때문에 0, 1 || factor을 사용하여 범주형으로 바꿔줘야 함

#factor을 이용하여 범주화를 함.
haberman$survival = factor(haberman$survival)

h = glm(survival ~ age + op_year + no_nodes, data = haberman, family = binomial)
# == h = glm(survival ~. , data = haberman, family = binomial)
coef(h)
deviance(h) #328.2564

#새로운 환자 두 명을 입력
new_patients = data.frame(age = c(37, 66), op_year = c(58, 60), no_nodes = c(5, 32))
#predict의 결과값으로 0 ~ 1사이의 survival 확률을 출력됨. 1 - 결과로 확률을 계산해야함 
#(사망일 경우가 2, 생존의 경우가 1이기 때문)
predict(h, newdata = new_patients, type = 'response') # 1 - 0.22 = 0.78 // 1 - 0.84 = 0.16


#특징선택 = 영향을 안준다고 판단하는 변수를 제거
h = glm(survival ~ age + no_nodes, data = haberman, family = binomial)
coef(h)
deviance(h) #328.3507 deviance값이 늘어나는 결과를 초래했으므로 이 경우는 특징선택이 유리하지 않다.
new_patients = data.frame(age = c(37, 66), op_year = c(58, 60), no_nodes = c(5, 32))
predict(h, newdata = new_patients, type = 'response')

#로지스틱 회귀
#반응변수는 0 또는 1이라는 두가지 값만 가진다.
#승리 패배, 성공 실패, True False와 같은 반응 변수를 다루는 회귀를 로지스틱 회귀라고 함
#(회귀 - 연속된 변수들에 대해 두 변수 사이의 모형을 구한 뒤 적합도를 측정하는 분석방법)
#선형 회귀 - l = a1X + a0 와 같은 모델로는 0 1로만 값을 가져야 하는 로지스틱 회귀를 제대로 모델링 할 수 없다.
#그래서 y = 1 / 1 + e^-l의 logit function으로 만들어서 0과 1사이의 반응변수를 가질 수 있도록 했다.

#UCLA admission
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
#admit - 합불여부 gre - 대학원 시험 점수 gpa - 학부 성적 rank - 출식 대학
str(ucla)
#options(max.print = 1000)
#options(check.bounds = F)
plot(ucla)

library(dplyr) #편리한 데이터 조작을 위해
library(ggplot2) #plotting graphics
search()

#aes - construct aesthetic mapping
#ggplot(data = ucla, aes(gre, admit)) + geom_point()
#ucla의 x축 gre, y축 admit을 geom_point로 출력
ucla %>% ggplot(aes(gre, admit)) + geom_point()

#overplotting 을 방지하기 위해서 geom_jitter을 이용하여 랜덤으로 점을 배치.
ucla %>% ggplot(aes(gre, admit)) + geom_jitter()

#admit의 값에 따라 color에 변화를 줌. 
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = admit))

#admit은 0 아니면 1의 값만을 가지기 때문에 factor을 이용하여 admit의 레벨(0, 1)에 따른 색을 출력하도록 함.
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = factor(admit)))

#가로축 jitter을 없얘고, 세로측 jitter의 양을 줄이기 위해서(to 10%) height, width값을 설정
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = factor(admit), cex = 3), height = 0.1, width = 0.0)

#두 개의 그래프를 한번에 출력하기 위한 라이브러리
library(gridExtra)
search()

#gpa, admit 관련 그래프 출력
p1 = ggplot(ucla, aes(gpa, admit)) + geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.0)

#rank, admit 관련 그래프 출력
p2 = ggplot(ucla, aes(rank, admit)) + geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.1)

#gridExtra 라이브러리를 이용하여 두 개를 한번에 출력
grid.arrange(p1, p2, ncol = 2)

#UCLA admission에 로지스틱 회귀를 적용
#m = glm(admit ~ gre + gpa + rank, data = ucla, family = binomial)
m = glm(admit ~ ., data = ucla, family = binomial)
coef(m) #log(odds) = a + bX1 + cX2 + dX3 ... 
#값의 범위에 따라 계수의 크기가 달라진다. gre는 범위가 넓기 때문에 계수가 적음 
#rank는 낮을수록 좋은 것이기 때문에 -가 붙음

s = data.frame(gre = c(376),
               gpa = c(3.6),
               rank = c(3))
predict(m, newdata = s, type = 'response') # 새로운 학생 합격확률 18%...




#survival라이브러리에 있는 colon 데이터에 일반화 선형 모델을 적용
library(survival)
search()

#colon is the data of first successful trials of adjuvant(보조) chemotherapy for colon cancer
#adjuvant(보조) chemotherapy = (수술, 방사선) + 약물치료 , colon cancer = 대장암
str(colon) #status를 추축할 것임(재발/사망 = 1, 완치 = 0)

plot(colon) #변수가 많아서 상관관계를 파악하기 어렵다

#extent(암세포의 침투깊이 1이 제일 깊다) - status 관계
p1 = ggplot(data = colon, mapping = aes(extent, status)) + geom_jitter(mapping = aes(col = factor(status)), height = 0.1, width = 0.1)
#age - status 관계
p2 = ggplot(data = colon, mapping = aes(age, status)) + geom_jitter(mapping = aes(col = factor(status)), height = 0.1, width = 0.1)
#sex - status 관계 
p3 = ggplot(data = colon, mapping = aes(sex, status)) + geom_jitter(mapping = aes(col = factor(status)), height = 0.1, width = 0.1)
#nodes(암세포가 있는 림프절 수) - status 관계
p4 = ggplot(data = colon, mapping = aes(nodes, status)) + geom_jitter(mapping = aes(col = factor(status)), height = 0.1, width = 0.1)
#4개 동시 출력
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

#status에 대한 로지스틱 함수를 적용
m = glm(status ~ ., data = colon, family = binomial)

#degrees of freedom - 값의 자유도 예) 평균이 A인데 값이 총 n개고 임의로 n - 1개의 값을 정할 수 있지만 마지막 값은 평균에 의해 정해져 버리므로 자유도는 n - 1 
#residual - 잔차

#null deviance - 2(LL(Saturated Model) - LL(Null Model)) // *LL = log likelihood
#saturated model = 각 데이터 포인트는 자신의 parameter을 가지고 있음 모두 최대의 값으로 곱해짐
#null model = 단 하나만의 parameter만으로 모든 데이터 포인트를 적용하여 모두 곱함

#null deviance가 작을수록 null model이 data를 잘 설명해준다고 볼 수 있다.

#residual deviance - 2(LL(Saturated Model) - LL(Proposed Model))
# 곱하기 2를 하는 이유는 log-likelihood의 차이가 parameter의 개수의 차이와 degrees of freedom을 갖는 Chi-squared 분포를 가지도록 함

#AIC - Akaike Information Criterion = 비슷한 모델간의 품질 비교 
#AIC는 관계없는 예측변수에 대한 개입을 방지하기 위해서 만들어짐. 두 개 이상의 AIC값중에 수치가 더 적을수록 좋다.
m

deviance(m) #잔차의 제곱합 666.32

table(is.na(colon)) #is.na()에 대한 테이블을 생성해준다 (개수를 구해줌)
#NA가 82개

#colon에 있는 결측값을 na.omit()을 이용하여 제거해줌
clean_colon = na.omit(colon)
m = glm(status ~., data = clean_colon, family = binomial)
m  #결과 데이터가 같다

#study, time, etype, id를 제외(특징선택)후 glm적용
clean_colon = clean_colon[c(T, F), ] #홀수만 취하는 것임 to get only 1 id per person

#이런 원리로 홀수가 선택됨
x = c(1, 2, 3, 4, 5, 6)
x = x[c(T, F)]
x

#이런 식으로 의미없는 설명변수를 제거해야
#study는 모든 샘플이 같은 1을 가지고 있으므로 무의미
#time과 같은 경우는 병의 사망시점을 알려주기 때문에 사망을 예측하는 부분에서는 time을 몰라야 한다
#id 는 관계가 없는 변수, etype의 사망/재발의 여부도 예측에 방해가 된다. 
m = glm(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4,
        data = clean_colon, family = binomial)
deviance(m)
m

str(colon)

