library(rpart)

iris

library(dplyr)
arrange(iris, iris$Petal.Length)

r = rpart(Species ~ ., data = iris) #왜 결과값이 ppt와 다르지?
print(r)

par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

#type = class - 개수로 분류, prob(default) - 각각의 데이터에 대한 확률 출력
p = predict(r, iris, type = 'class') #만들어놓은 결정트리로 iris데이터를 classification타입으로 추측작업을 함.

#다수의 분류된 species들을 table화함.
table(p, iris$Species) # 6/150의 오류율

#parms parameter in rpart
#For classification splitting, the list can contain any of: the vector of prior probabilities (component prior), the loss matrix (component loss) or the splitting index (component split). 
#The priors must be positive and sum to 1. The loss matrix must have zeros on the diagonal and positive off-diagonal elements. The splitting index can be gini or information. 
#The default priors are proportional to the data counts, the losses default to 1, and the split defaults to gini.
#prior probability - 미리 class별 발생 확률을 알 경우에 기본 확률을 정해줌
#setosa : versicolor : virginica
#이로 인해서 랜덤으로 분류할시 33.33..%의 확률로 분류가 되지만 해당 예제에서는 랜덤으로 분류를 해도 80%의 분류확률을 확보할 수 있다.
r_prior = rpart(Species ~ ., data = iris, parms = list(prior = c(0.1, 0.1, 0.8)))
#setosa 0 / versicolor 6 / virginica 1 인데 virginica로 분류가 된 이유는 정확률 80%로 virginica이기 때문
plot(r_prior)
text(r_prior, use.n = T)

newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), 
                  Sepal.Width = c(3.51, 3.2, 3.31), 
                  Petal.Length = c(1.4, 4.71, 6.02), 
                  Petal.Width = c(0.19, 1.4, 2.49))
print(newd)
#n번째 샘플의 각 class별 확률을 출력함
predict(r, newdata = newd, type = 'prob')

summary(r)
#variable importance 특징선택 - 필요없어보이는 변수를 사전에 임의로 제거
#여기서는 중요도 순으로 보여줌.
#root node - 0, 몇개의 샘플이 도달(observation)했는지와 부류 확률(probabilities)을 알려줌

library(rpart.plot) #효과적인 시각화를 위한 라이브러리
rpart.plot(r)

#'rpart.plot types...
#'0 Draw a split label at each split and a node label at each leaf.
#'1 Label all nodes, not just leaves. Similar to text.rpart's all=TRUE.
#'2 Default. Like 1 but draw the split labels below the node labels. Similar to the plots in the CART book.
#'3 Draw separate split labels for the left and right directions.
#'4 Like 3 but label all nodes, not just leaves. Similar to text.rpart's fancy=TRUE. See also clip.right.labs.
#'5 New in version 2.2.0. Show the split variable name in the interior nodes.
rpart.plot(r, type = 4)
?rpart.plot

#높은 성능을 위해서 앙상블 기법(다수의 모델을 만들고 이를 결합하여 더 좋은 예측을 하게함)을 이용한 랜덤 포리스트를 사용
#여러개의 결정트리의 결과를 추출하여 다수결로 정함
library(randomForest)

search()
# 118학점 3.32 
# 39학점 n -> 157학점 3.5 
# 3.32*118 + 39n = 3.5*157

f = randomForest(Species ~ ., data = iris)
f #confusion matrix에 의하면 150개중 6개의 샘플을 잘못 분류함. --> 정확도 96%
summary(f) #더 자세한 정보를 볼 수 있다.
plot(f) #검정은 평균 오류율 나머지는 각 class별 오류율


varUsed(f) #설명 변수가 질문에 사용된 횟수
varImpPlot(f) #설명 변수의 중요도를 그래프화

treesize(f) #만들어진 다수의 결정트리 각각의 리프노드의 개수를 알려줌

#만들어진 랜덤포리스트로 새로운 샘플을 예측해보자
newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32),
                  Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02),
                  Petal.Width = c(0.19, 1.4, 2.49))
predict(f, newdata = newd) #랜덤 포리스트는 결정 트리가 확률을 출력해준 것과는 다르게 classification 정보만 출력함
predict(f, newdata = newd, type = 'prob') #확률 정보를 출력
predict(f, newdata = newd, type = 'vote', norm.votes = F) #투표 수를 출력함

#'하이퍼 매개변수
#'모델의 구조나 학습방법을 제어
#'random forest의 하이퍼 매개변수
#'ntree - 결정트리의 개수(default = 500)
#'nodesize - 리프노드에 도달한 샘플의 최소 개수(개수가 클수록 결정트리가 작아짐)
#'maxnodes - 리프노드의 최대 개수
#'
#'cost - c를 크게 할수록 오류를 허용하지 않아 일반화능력이 안좋아짐
#'대신 새로운 데이터에 대한 성능이 낮아짐
small_forest = randomForest(Species ~ ., data = iris, ntree = 20, nodesize = 6, maxnodes = 12)
treesize(small_forest)

library(e1071) #SVM을 사용하기 위해 쓰는 라이브러리
#SVM은 두 부류로 분류하는 이진 분류 모델임

#'SVM 커널함수 - 어떤 모양의 선으로 나누느냐!
#'polynomial(다항식)
#'radial basis function(default)(방사기저 함수 - 신경망에 사용)
#'sigmoid(s자 모양의 logistic function)
s = svm(Species ~ ., data = iris) 
print(s)
table(predict(s, iris), iris$Species) # 4/150의 오류율

s = svm(Species ~ ., data = iris, kernel = 'polynomial')
p = predict(s, iris)
table(p, iris$Species) # 7/150의 오류율을 보이므로 polynomial보다 radial basis가 더 성능이 높았다.

#cost를 정해주어 오류율을 줄임 
s = svm(Species ~ ., data = iris, cost = 100)
p = predict(s, iris)
table(p, iris$Species) # 2/150의 오류율로 줄어듬

#'SVM의 기존 이진 분류모델을 이용
#'SVM을 k개 만들어서 K-nn으로 확장
#'새로운 샘플이 입력되면 샘플과 가장 가까운 k개의 훈련집합 샘플을 찾고 
#'발생빈도가 가장 높은 부류로 분류함(?)
library(class)
train = iris
test = data.frame(Sepal.Length = c(5.11, 7.01, 6.32),
                  Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02),
                  Petal.Width = c(0.19, 1.4, 2.49))

train[, 1:4] #Species를 제거한 값
#knn(train data, 예측할 new data, GroundTruth, k의 개수)
k = knn(train[, 1:4], test, train$Species, k = 5)


library(caret) #knn, rpart등의 기능을 train()에 추가하여 일관성을 도모
r = train(Species ~ ., data = iris, method = 'rpart') #결정트리
f = train(Species ~ ., data = iris, method = 'rf') #random forest
s = train(Species ~ ., data = iris, method = 'svmRadial') #support vector machine분류법(아직 안배움)
k = train(Species ~ ., data = iris, method = 'knn') #knn

ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit) #ucla데이터 factor로 해서 rpart 와 randomForest가 돌아갈 수 있는 기반을 제공
#factor을 하지 않을시에는 회귀로 작동.

#합불여부를 알려주는 결정트리를 생성
r = rpart(admit ~ ., data = ucla)
r = train(admit ~ ., data = ucla, method = 'rpart')

#그래픽 parameter
#그래프를 정해진 vector값에 따라서 row로 출력(입력 row, 출력 row)
#바뀔일이 없는데 왜 쓴건지 모르겠음
par(mfrow = c(1, 1), xpd = NA) #NA - all plotting is clipped to the device region
plot(r)
text(r, use.n = T)

p = predict(r, ucla, type = 'class') #결정트리를 이용하여 ucla데이터의 class별 예측
table(p, ucla$admit) #테이블로 보여줌

#합불여부를 알려주는 randomForest를 생성
f = randomForest(admit ~ ., data = ucla)
f = train(admit ~ ., data = ucla, method = 'rf') 
print(f)

#colon 데이터를 이용한 분류
library(survival)
clean_colon = na.omit(colon) #결측값 제거
clean_colon = clean_colon[c(T, F), ] #홀수 값만 취함
clean_colon$status = factor(clean_colon$status) #factor화함
str(clean_colon)

#11개의 변수를 선택하여 사용. 선택변수
r = rpart(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
p = predict(r, clean_colon, type = 'class')
table(p, clean_colon$status)

plot(r)
text(r, use.n = T)

summary(r)

#11개의 변수를 선택하여 사용. 선택변수
f = randomForest(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
print(f)

#Gender Recognition
voice = read.csv('/Users/hyunjinkim/selfStudy-R/voice.csv')
table(is.na(voice)) #결측치 x
?train
r = rpart(label ~ ., data = voice)
r = train(label ~ ., data = voice, method = 'rpart')
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

#오류율을 보기 위해 classification 한 테이블을 생성
p = predict(r, voice, type = 'class')
table(p, voice$label) # (88 + 33) / 3168

#randomForest로 예측
f = randomForest(label ~ ., data = voice)
print(f)

