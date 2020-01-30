#word2vec은 단어를 벡터로 변환하는 기술이다.

example = 'In the field of computer science, artificial intelligence(AI), sometimes called machine intelligence, is intelligence demonstrated by machines, in contrast to the natural intelligence displayed by humans and other animals.'

doc = Corpus(VectorSource(example))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english')) #불용어 제거
doc = tm_map(doc, removePunctuation) #구두점(특수문자) 제거
doc = tm_map(doc, stripWhitespace) # 단어를 없얘면서 생긴 공백 문자를 제거함

#DWM대신 DTM이라고 하는 이유는 n-그램을 사용하기 때문, 단어의 순서정보를 보완할 수 있다.

#'DTM의 추가 설명
#'1. 사전 구축
#'실제로 사전을 구축할때, 크기는 수만~수십만이다.
#'말뭉치에서 추출하지 말고 국어사전에서 있는 모든 단어를 사전으로 사용할 수 있다.
#'
#'2. 문서가 벡터로 표현되므로 거리 측정가능
#'https://rfriend.tistory.com/319 - cosine Distance 측정 법 
#'거리가 측정가능하므로 Random Forest, SVM 유사도 측정도 가능해짐 
#'
#'3. 문서가 긴 경우 단어의 발생빈도가 높아 벡터를 구성하는 요소의 값이 커지기 때문에 벡터의 크기가 커짐으로 인해서 유사한 문서와 거리가 멀어지는 문제점이 발생
#' 벡터의 크기를 1로 만드는 정규화를 수행함. 근데 그걸로 어떻게 계산할건지 나는 모르겠음.
#'
#'4. DTM은 희소 행렬
#'한번도 발생하지 않아서 0인 칸이 매우 많다. 
#'
#'5. DTM은 단어사이의 상호작용을 표현할 때 제약사항이 있다.
#'n-gram을 이용해서 해결이 가능하지만 열의 개수가 기하급수적으로 늘어남 why?
#'https://web.stanford.edu/~jurafsky/slp3/3.pdf


library(RCurl) #web server에 접속하기 위한 라이브러리

library(XML) # 웹 문서를 처리하기 위한 라이브러리 Read || Create XML files..

t = readLines('https://en.wikipedia.org/wiki/Data_science') #html파일을 읽어옴
str(t) #vector char
d = htmlParse(t, asText = T) #web문서를 파싱하여 vector로 나누어진 HTML을 Class형으로 합쳐준다(XML library)
str(d) #Classes type
clean_doc = xpathSApply(d, "//p", xmlValue) #html 스크립트 코드를 제거 xpathSApply를 하려면 Class형이여야함
str(clean_doc) #vector char

library(tm) #데이터마이닝 함수 제공 
library(SnowballC) #어간을 추출하는 함수 제공 없어도 잘 돌아감 사용하는 함수가 없는데 왜 쓰는거
search()

vsDoc = VectorSource(clean_doc)
vsDoc #attr(, "class")아래에 나오는 건 상속받은 클래스들 

doc = Corpus(VectorSource(clean_doc)) # clean_doc을 vectorSource화하여 문서의 집합(corpus)로 묶어준다
doc #corpus로 묶었기 때문에 보이지 않는다.
inspect(doc) #를 사용하여 doc의 내용을 볼 수 있음.

doc = tm_map(doc, content_transformer(tolower)) #tm_map은 지정된 매개변수 값에 따라서 전처리를 수행.
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc) #DTM을 구해줌
dim(dtm) #dim함수는 DTM의 행과 열의 개수를 알려줌

#'Non-/sparse entries: 0이 아닌 개수 / (documents개수 * terms개수 = table의 칸 개수)
#'Sparsity: 전체 데이터에서 0인 것의 퍼센티지
#'Maximal term length: term matrix에서 가장 긴 단어의 length
#'Weighting: 가중치 방식?
inspect(dtm) #inspect 함수는 상세내용을 요약하여 보여줌

lookAll = as.data.frame(as.matrix(dtm)) #데이터 프레임으로 만들어서 lookAll에 넣으면 전체 matrix를 볼 수 있다. 
write.csv(lookAll, file = "dtm.csv") #csv 파일로 출력하면 더 간편하게 볼 수 있다.
##여기까지 
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




