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
table(p, iris$Species)

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


varUsed(f)
varImpPlot(f)

treesize(f)

newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32),
                  Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02),
                  Petal.Width = c(0.19, 1.4, 2.49))
predict(f, newdata = newd)

predict(f, newdata = newd, type = 'prob')
predict(f, newdata = newd, type = 'vote', norm.votes = F)

small_forest = randomForest(Species ~ ., data = iris, ntree = 20, nodesize = 6, maxnodes = 12)
treesize(small_forest)

library(e1071)
s = svm(Species ~ ., data = iris)
print(s)

table(predict(s, iris), iris$Species)

s = svm(Species ~ ., data = iris, kernel = 'polynomial')
p = predict(s, iris)
table(p, iris$Species)

s = svm(Species ~ ., data = iris, cost = 100)
p = predict(s, iris)

library(class)
train = iris
test = data.frame(Sepal.Length = c(5.11, 7.01, 6.32),
                  Sepal.Width = c(3.51, 3.2, 3.31),
                  Petal.Length = c(1.4, 4.71, 6.02),
                  Petal.Width = c(0.19, 1.4, 2.49))

k = knn(train[, 1:4], test, train$Species, k = 5)
k

library(caret)
search()
r = train(Species ~ ., data = iris, method = 'rpart')
f = train(Species ~ ., data = iris, method = 'rf')
s = train(Species ~ ., data = iris, method = 'svmRadial')
k = train(Species ~ ., data = iris, method = 'knn')

ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit)

r = rpart(admit ~ ., data = ucla)
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

p = predict(r, ucla, type = 'class')
table(p, ucla$admit)
search()
f = randomForest(admit ~ ., data = ucla)
print(f)

library(survival)
clean_colon = na.omit(colon)
clean_colon = clean_colon[c(T, F), ]
clean_colon$status = factor(clean_colon$status)
str(clean_colon)

r = rpart(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
p = predict(r, clean_colon, type = 'class')
table(p, clean_colon$status)

plot(r)
text(r, use.n = T)

summary(r)

f = randomForest(status ~ rx + sex + age + obstruct + perfor + adhere + nodes + differ + extent + surg + node4, data = clean_colon)
print(f)

voice = read.csv('/Users/hyunjinkim/selfStudy-R/voice.csv')
table(is.na(voice))

r = rpart(label ~ ., data = voice)
par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

p = predict(r, voice, type = 'class')
table(p, voice$label)

f = randomForest(label ~ ., data = voice)
print(f)














