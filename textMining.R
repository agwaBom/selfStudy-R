library(RCurl)
library(XML)

t = readLines('https://en.wikipedia.org/wiki/Data_science') #html파일을 읽어옴
t
d = htmlParse(t, asText = T) #web문서를 R데이터 형으로 변환
d
clean_doc = xpathSApply(d, "//p", xmlValue) #html 스크립트 코드를 제거
clean_doc

library(tm) #데이터마이닝 함수 제공 
library(SnowballC) #어간을 추출하는 함수 제공

doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower)) #tm_map은 지정된 매개변수 값에 따라서 전처리를 수행.
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm) #dim함수는 DTM의 행과 열의 개수를 알려줌

inspect(dtm) #inspect 함수는 상세내용을 요약하여 보여줌

library(wordcloud)

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = T)
d = data.frame(word = names(v), freq = v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = F, rot.per = 0.35)

library(RColorBrewer)
pal = brewer.pal(11, "Spectral")
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = F, rot.per = 0.50, colors = pal)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = F, rot.per = 0.50, colors = pal, family = "mono", font = 2)

library(wordcloud2)
wordcloud2(d)

d1 = d[1:200, ]
wordcloud2(d1, shape = 'star')
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0)

findFreqTerms(dtm, lowfreq = 12)
findAssocs(dtm, terms = 'harvard', corlimit = 0.7)
barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10, ]$word, col = 'lightblue', main = '발생 빈도 상위 단어', ylab = '단어 빈도')

library(gapminder)
library(dplyr)

pop_siz = gapminder%>%filter(year == 2007)%>%group_by(continent)%>%summarize(sum(as.numeric(pop)))
d = data.frame(word = pop_siz[, 1], freq = pop_siz[, 2])
wordcloud(words = d[, 1], freq = d[, 2], min.freq = 1, max.words = 100, random.order = F, rot.per = 0.35)
wordcloud2(d)

library(text2vec)
library(caret)

str(movie_review)

head(movie_review)

#데이터를 훈련 집합과 테스트 집합으로 나눔.
train_list = createDataPartition(y = movie_review$sentiment, p = 0.6, list = F)
mtrain = movie_review[train_list, ]
mtest = movie_review[-train_list, ]

#훈련 집합으로 DTM구축
doc = Corpus(VectorSource(mtrain$review))
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm) #사전의 크기는 36933(3000개의 문서에서 36933개의 단어가 추출됨)

inspect(dtm)
#Non-/sparse entries: 295267/110503733
#110799000 = 3000 * 36933
#110799000개의 칸중 295267개만이 0이 아니고 나머지 110503733개는 0 

#사전이 아주 커서 메모리 오류 발생 -> removeSparseTerms함수로 줄임
#cbind 함수로 반응 변수 sentiment를 덧붙임
dtm_small = removeSparseTerms(dtm, 0.90)
X = as.matrix(dtm_small)
dataTrain = as.data.frame(cbind(mtrain$sentiment, X))
dataTrain$V1 = as.factor(dataTrain$V1)
colnames(dataTrain)[1] = 'y'

library(rpart)
r = rpart(y ~ ., data = dataTrain)
printcp(r)

par(mfrow = c(1, 1), xpd = NA)
plot(r)
text(r, use.n = T)

library(randomForest)
f = randomForest(y ~ ., data = dataTrain)

#테스트 집합으로 DTM구축
docTest = Corpus(VectorSource(mtest$review))
docTest = tm_map(docTest, content_transformer(tolower))
docTest = tm_map(docTest, removeNumbers)
docTest = tm_map(docTest, removeWords, stopwords('english'))
docTest = tm_map(docTest, removePunctuation)
docTest = tm_map(docTest, stripWhitespace)

dtmTest = DocumentTermMatrix(docTest, control = list(dictionary = dtm_small$dimnames$Terms))
dim(dtmTest)
str(dtmTest)
inspect(dtmTest)

X = as.matrix(dtmTest)
dataTest = as.data.frame(cbind(mtest$sentiment, X))
dataTest$V1 = as.factor(dataTest$V1)
colnames(dataTest)[1] = 'y'
pr = predict(r, newdata = dataTest, type = 'class') #결정 트리 r
table(pr, dataTest$y) #651 + 694 / 2000의 정확률

pf = predict(f, newdata = dataTest) #랜덤 포리스트 f
table(pf, dataTest$y) #687 + 763 / 2000의 정확률

library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)
t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = T)
clean_doc = xpathApply(d, "//p", xmlValue)

#전처리 수행
doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm) #41개의 문장을 각각 문서로 간주하여 39개의 문서로 추출한다.
#이들 문서에서 1533개의 단어를 추출하여 사전 구축

inspect(dtm) #등, 있다 등을 단어로 추출하였고, 데이터, 데이터를, 데이터의를 다른 단어로 추출하는 한계

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = T)
d = data.frame(word = names(v), freq = v)
d1 = d[1:500, ] #500개의 단어만 표시
wordcloud2(d1) #영어 텍스트 마이닝을 한글에 적용했기 떄문에 '데이터의'와 같은 게 다른 단어로 간주되어 중요한 자리를 차지함.

library(KoNLP) #한국어 전용 텍스트 마이닝 라이브러리
library(rJava)
Sys.setenv(JAVA_HOME = '/Library/Java/JavaVirtualMachines/jdk1.8.0_241.jdk/Contents/Home')

Sys.getenv("JAVA_HOME")

useSystemDic()
useSejongDic()
useNIADic()

useSejongDic()
s = '너에게 묻는다 연탄재 함부로 발로 차지 마라 너는 누구에게 한번이라도 뜨거운 사람이었느냐'
extractNoun(s)
simplePoss22(s)


t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = T)
clean_doc = xpathApply(d, "//p", xmlValue)

useSejongDic()

nouns = extracNoun(clean_doc)
mnous = unlist(nouns)
mnous_freq = table(mnous)
v = sort(mnous_freq, decreasing = T)

wordcloud2(v) #모든 단어 표시
v1 = v[1:100]
wordcloud2(v1) #상위 100개의 단어만 표시




