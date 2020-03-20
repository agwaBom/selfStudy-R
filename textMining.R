#word2vec은 단어를 벡터로 변환하는 기술이다.

example = 'In the field of computer science, artificial intelligence(AI), 
sometimes called machine intelligence, is intelligence demonstrated by machines, 
in contrast to the natural intelligence displayed by humans and other animals.'

doc = Corpus(VectorSource(example)) #example을 vectorSource화하여 문서의 집합(corpus)로 묶어준다

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english')) #불용어 제거
doc = tm_map(doc, removePunctuation) #구두점(특수문자) 제거
doc = tm_map(doc, stripWhitespace) # 단어를 없얘면서 생긴 공백 문자를 제거함

inspect(doc)

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
#'n-gram을 이용해서 해결이 가능하지만 열의 개수가 기하급수적으로 늘어남
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
library(SnowballC) #어간을 추출하는 함수 제공 없어도 잘 돌아감 사용하는 함수가 없어서 쓸 이유가 없다.
search()

vsDoc = VectorSource(clean_doc)
vsDoc #attr(, "class")아래에 나오는 건 상속받은 클래스들 

doc = Corpus(VectorSource(clean_doc)) #clean_doc을 vectorSource화하여 문서의 집합(corpus)로 묶어준다
doc #corpus로 묶었기 때문에 보이지 않는다.
inspect(doc) #를 사용하여 doc의 내용을 볼 수 있음.

doc = tm_map(doc, content_transformer(tolower)) #tm_map은 지정된 매개변수 값에 따라서 전처리를 수행.
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc) #DTM을 구해줌
dim(dtm) #dim함수는 DTM의 행과 열의 개수를 알려줌

#'documents개수 * terms개수 = table의 칸 개수
#'Non-/sparse entries: 0이 아닌 개수 / 0인 것의 개수
#'Sparsity: 전체 데이터에서 0인 것의 퍼센티지
#'Maximal term length: term matrix에서 가장 긴 단어의 length
#'Weighting: 가중치 방식?
inspect(dtm) #inspect 함수는 상세내용을 요약하여 보여줌

lookAll = as.data.frame(as.matrix(dtm)) #데이터 프레임으로 만들어서 lookAll에 넣으면 전체 matrix를 볼 수 있다. 
write.csv(lookAll, file = "dtm.csv") #csv 파일로 출력하면 더 간편하게 볼 수 있다.

##여기까지 
library(wordcloud)

m = as.matrix(dtm) #dtm을 matrix로 변환
v = sort(colSums(m), decreasing = T) #colSums() - 해당 열을 모두 더해줌, 빈도가 높은 순으로 단어를 정렬.
d = data.frame(word = names(v), freq = v) #데이터프레임으로 변환 v는 named number이기 때문에 word에는 v의 이름들, freq에는 v의 숫자를 넣을 수 있다. 
d # 더 많이 출력하려면 option을 조정하면 됨. options(max.print = 1000)
#'rot.per - 세로로 배치할 단어의 비율
#'상위 100개의 단어를 출력함
#'random.order = F이면 중간이 제일 크고 밖으로 나갈수록 적은거.
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = F, rot.per = 0.35) 

library(RColorBrewer) #wordcloud에 색상을 입히기 위한 라이브러리

pal = brewer.pal(11, "Spectral") #spectral은 11가지의 색상을 제공함
#brewer.pal.info 를 이용하면 사용가능한 팔레트의 종류와 색의 개수를 볼 수 있다.
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, 
          random.order = F, rot.per = 0.50, colors = pal)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, 
          random.order = F, rot.per = 0.50, colors = pal, family = "mono", font = 3) 
#family를 통해서 폰트를 설정, font 를 통해서 폰트 종류(1 - plain, 2 - bold, 3 - italic, 4 - bold italic)를 결정 ?par

library(wordcloud2) #wordcloud의 새로운 버전.
wordcloud2(d) #자동으로 색을 입히고, 다양한 방향으로 표시할 수 있다.

d1 = d[1:200, ] #wordcloud와 같이 단어의 개수를 지정할 수 없기 때문에 사전에 상위 200개로 단어의 수를 한정시킴.
wordcloud2(d, shape = 'star', size = 5) #shape를 이용하여 모양을 지정할 수 있다.
#rotateRatio = 1이면 모든 단어가 min ~ max사이의 Rotation 값으로 set.
#즉 rotate시킬 비율이 100%라는 것임.
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0, size = 6) 
d1

findFreqTerms(dtm, lowfreq = 12) #발생 빈도가 12이상인 단어만 보여준다.
findAssocs(dtm, terms = 'harvard', corlimit = 0.7) #상관관계 큰 단어순으로 출력 (DTM에 의한 vector 거리 계산)
#las로 x y축의 단어 배치방향 조절  0 - x 가로, y 세로 / 1 - x 가로, y 가로 / 2 - x 가로, y 세로 / 3 - x 세로, y 세로
barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10, ]$word, col = 'lightblue', main = '발생 빈도 상위 단어', ylab = '단어 빈도') 


library(gapminder) #각 연도에 의한 나라별 기대 수명, GDP per capita - GDP/POPULATION, population
#select() 특정 열 추출
#filter() 특정 행 추출
#summarise() 그룹별 통계 지표를 추출
#arrange() 행 배열 순서를 변경
#group_by() factor(범주형)을 이용하여 전체 데이터를 그룹화할 수 있음
#...etc

library(dplyr) # 편리한 데이터 가공을 위해
# %>% 연산자를 이용하여 데이터를 연속 처리.
#' tmp = filter(gapminder, year == 2007) - 2007년에 해당하는 전체 행 추출
#' tmp1 = group_by(tmp, continent) - continent별로 그룹화
#' pop_siz = summarize(tmp1, sum(as.numeric(pop))) - tmp1$pop의 int(1L...정수) 타입을 numeric(실수)으로 변환후 continent별로 더해줌
pop_siz = gapminder%>%filter(year == 2007)%>%group_by(continent)%>%summarize(sum(as.numeric(pop)))
d = data.frame(word = pop_siz[, 1], freq = pop_siz[, 2]) #대륙과 대륙별 인구수의 합에 대한 데이터 프레임을 생성함
wordcloud(words = d[, 1], freq = d[, 2], min.freq = 1, max.words = 100, random.order = F, rot.per = 0.35)
#rot.per 세로로 배치할 단어의 비율
wordcloud2(d)

#본래는 빠른 vector화, 토픽 모델링을 위한 라이브러리인데 베타버전임
#http://text2vec.org
library(text2vec) #movie_review 데이터를 사용하기 위한 라이브러리
library(caret) #모델의 성능 평가를 위한 라이브러리

#'id - 일런번호
#'sentiment - 1 - 긍정평가, 2 - 부정평가
#'review - 영화 리뷰
str(movie_review)
head(movie_review) #둘다 요약인데 head에서는 review의 전문을 보여줌
View(movie_review) #전체 내용을 보고 싶으면 이걸 이용하면 됨. 

#데이터를 6:4의 비율의 훈련 집합과 테스트 집합으로 나눔.
#sentiment를 고려하여 동일한 sentiment비율을 가질 수 있도록 나누는 것임
train_list = createDataPartition(y = movie_review$sentiment, p = 0.6, list = F) 
?createDataPartition
movie_review$sentiment
train_list
mtrain = movie_review[train_list, ] #movie_review$sentiment의 60%인 train_list 세트
mtest = movie_review[-train_list, ] #train_list를 제외한 40%의 movie_review 세트

#mtrain$sentiment의 데이터 비율
i = 1
total = 0
while(i <= length(mtrain$sentiment)){
  total = total + mtrain$sentiment[i]
  i = i + 1
}
mtrainRatio = total/length(mtrain$sentiment)
mtrainRatio # 0.5116667

#mtest$sentiment의 데이터 비율
i = 1
total = 0
while(i <= length(mtest$sentiment)){
  total = total + mtest$sentiment[i]
  i = i + 1
}
mtestRatio = total/length(mtest$sentiment)
mtestRatio # 0.491

#훈련 집합으로 DTM구축
doc = Corpus(VectorSource(mtrain$review))
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc) #DTM 구축
dim(dtm) #사전의 크기는 36933(3000개의 문서에서 36933개의 단어가 추출됨) createDataPartition에 의해 매번 달라짐 
inspect(dtm)
#Non-/sparse entries: 295267/110503733
#110799000 = 3000 * 36933
#110799000개의 칸중 295267개만이 0이 아니고 나머지 110503733개는 0
#sparsity - 2/1105이므로 거의 100% 0임.

#사전이 아주 커서 메모리 오류 발생 -> removeSparseTerms함수로 sparse(0)인 것을 줄임
#cbind 함수로 반응 변수 sentiment를 덧붙임
dtm_small = removeSparseTerms(dtm, 0.90) #빈도가 일정 이하인 단어의 90%를 제거 
dtm_small #110503733개에서 256233개로 전체 사전의 크기가 줄어듦 하지만 nonSparse의 개수도 줄어들었다 
# -> 어느정도 손실은 감안해야함.
X = as.matrix(dtm_small) #dtm_small을 matrix화함
View(X)
#matrix화한 dtm_small과 mtrain$sentiment의 데이터를 바인딩한(V1) 데이터 프레임을 생성
dataTrain = as.data.frame(cbind(mtrain$sentiment, X)) 
dataTrain$V1 = as.factor(dataTrain$V1) #V1을 기준으로 factor화 함
colnames(dataTrain)[1] = 'y' #V1을 y로 이름을 바꿈

library(rpart) #결정트리 라이브러리
r = rpart(y ~ ., data = dataTrain) #dataTrain을 이용한 훈련

#'Root node error: 
#' CP - 복잡도(정해진 질문에 맞게 잘 나눠지면 CP가 낮아짐)
#' nsplit - 분기
#' rel error - 오류율
#' xerror - 교차 검증 오류
#' xstd - 표준 오차
printcp(r)
?par
#mfrow row로 그래프 분할https://rfriend.tistory.com/151
#xpd 
#NA - all plotting is clipped to plot region
#T - all plotting is clipped to figure region 
#F - all plotting is clipped to device region 얘는 잘렸음
par(mfrow = c(1, 1), xpd = NA) 
plot(r)
text(r, use.n = T)

library(randomForest) #랜덤포리스트 라이브러리
f = randomForest(y ~ ., data = dataTrain) #훈련 데이터를 이용하여 랜덤포리스트 학습 


#테스트 집합 이용하여 성능평가를 하기 위해 DTM구축
docTest = Corpus(VectorSource(mtest$review))
docTest = tm_map(docTest, content_transformer(tolower))
docTest = tm_map(docTest, removeNumbers)
docTest = tm_map(docTest, removeWords, stopwords('english'))
docTest = tm_map(docTest, removePunctuation)
docTest = tm_map(docTest, stripWhitespace)

#DTM의 구축에 있어서 사전이 훈련된 데이터의 사전과 동일해야 하므로 control옵션을 이용하여 사전을 가져옴
dtmTest = DocumentTermMatrix(docTest, control = list(dictionary = dtm_small$dimnames$Terms))
dim(dtmTest)
str(dtmTest)
inspect(dtmTest) #document의 개수 6:4로 훈련집합에 비해 더 적기 때문에 matrix의 총 크기 또한 적어짐

#예측을 시키기 위해서 mtest$sentiment값을 바인딩함
X = as.matrix(dtmTest)
dataTest = as.data.frame(cbind(mtest$sentiment, X))
dataTest$V1 = as.factor(dataTest$V1)
colnames(dataTest)[1] = 'y'

#훈련된 모델을 이용하여 새로운 데이터인 dataTest(테스트 테이터)넣어 성능 확인
pr = predict(r, newdata = dataTest, type = 'class') #결정 트리 r
table(pr, dataTest$y) #545 + 798 / 2000의 정확률

pf = predict(f, newdata = dataTest) #랜덤 포리스트 f
table(pf, dataTest$y) #663 + 749 / 2000의 정확률


#영어 텍스트 마이닝을 이용한 한국어 처리
library(tm) #데이터마이닝 함수 제공 
library(XML) # 웹 문서를 처리하기 위한 라이브러리 Read || Create XML files..
library(wordcloud2)
library(SnowballC) #필요없음
library(RCurl) #web server에 접속하기 위한 라이브러리
search()

t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = T)
clean_doc = xpathApply(d, "//p", xmlValue)

#전처리 수행
doc = Corpus(VectorSource(clean_doc)) # clean_doc을 vectorSource화하여 문서의 집합(corpus)로 묶어준다
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)
typeof(doc)
dtm = DocumentTermMatrix(doc)
dim(dtm) #41개의 문장을 각각 문서로 간주하여 41개의 문서로 추출한다.
#이들 문서에서 1533개의 단어를 추출하여 사전 구축

inspect(dtm) #등, 있다 등을 단어로 추출하였고, 데이터, 데이터를, 데이터의를 다른 단어로 추출하는 한계

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = T)
d = data.frame(word = names(v), freq = v)
d1 = d[1:500, ] #500개의 단어만 표시
#영어 텍스트 마이닝을 한글에 적용했기 떄문에 '데이터의'와 같은 게 다른 단어로 간주되어 중요한 자리를 차지함.
wordcloud2(d1)

#KoNLP를 이용한 한국어 텍스트 마이닝
library(KoNLP) #한국어 전용 텍스트 마이닝 라이브러리
useSystemDic() #28만 단어의 사전
useSejongDic() #37만 단어
useNIADic() #98만 단어

useSejongDic() #세종 사전을 불러온다
s = "너에게 묻는다 연탄재 함부로 발로 차지 마라 너는 누구에게 한번이라도 뜨거운 사람이었느냐."
extractNoun(s) #한글 텍스트에서 명사를 추출
SimplePos22(s)

#KoNLP를 이용한 단어 구름 그리기
t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = T)
clean_doc = xpathApply(d, "//p", xmlValue)

useSejongDic()

nouns = extractNoun(clean_doc)
mnous = unlist(nouns)
mnous_freq = table(mnous)
mnous_freq
v = sort(mnous_freq, decreasing = T)
wordcloud2(v) #모든 단어 표시
v1 = v[1:100]
wordcloud2(v1) #상위 100개의 단어만 표시




