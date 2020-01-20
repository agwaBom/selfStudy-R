library(rpart)

iris

library(dplyr)
arrange(iris, iris$Petal.Length)

r = rpart(Species ~ ., data = iris) #왜 결과값이 ppt와 다르지?
print(r)

par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

p = predict(r, iris, type = 'class') #만들어놓은 결정트리로 iris데이터를 class타입으로 추측작업을 함.
table(p, iris$Species)
p

str(iris)
?rpart

#parms parameter in rpart
#For classification splitting, the list can contain any of: the vector of prior probabilities (component prior), the loss matrix (component loss) or the splitting index (component split). 
#The priors must be positive and sum to 1. The loss matrix must have zeros on the diagonal and positive off-diagonal elements. The splitting index can be gini or information. 
#The default priors are proportional to the data counts, the losses default to 1, and the split defaults to gini.
r_prior = rpart(Species ~ ., data = iris, parms = list(prior = c(0.1, 0.1, 0.8)))
plot(r_prior)
text(r_prior, use.n = T)

newd = data.frame(Sepal.Length = c(5.11, 7.01, 6.32), 
                  Sepal.Width = c(3.51, 3.2, 3.31), 
                  Petal.Length = c(1.4, 4.71, 6.02), 
                  Petal.Width = c(0.19, 1.4, 2.49))

print(newd)
predict(r, newdata = newd, type = 'prob')

summary(r)

library(rpart.plot)
rpart.plot(r)
rpart(r, type = 4)

library(randomForest)
search()

f = randomForest(Species ~ ., data = iris)
f
summary(f)

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
table(p, iris$)

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














