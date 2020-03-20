library("igraph")
library(dplyr)
library(KoNLP)
library(tm)
useNIADic()
library(stringr)
library(rlist)
library(arules)


# c(2, 3) - 두번째 세번째 열이 주제랑 본문임.
delColArticle[1, c(2, 3)] 

# exNoun함수를 이용하여 단어 추출
nouns = sapply(delColArticle[1, c(2, 3)], exNoun)

# 문서 하나 전처리
myCorpus = Corpus(VectorSource(nouns))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeWords, stopwords('en'))

# Corpus를 plaintext화함 
myCorpus_txt = tm_map(myCorpus, PlainTextDocument)

# TermDocumentMatrix로 만들면서 wordlength를 정하여 한 글자 제거
myCorpus_txt = TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, Inf)))

# 데이터 프레임으로 변환
myTerm = as.data.frame(as.matrix(myCorpus_txt))

# 데이터 프레임에서 TF를 제거한 단어만 추출
wordlist = list(rownames(myTerm))
wordlist[3]= list(rownames(myTerm))


library(dplyr)
setwd("/Users/hyunjinkim/TransectionData")
# news 데이터 읽기
article = read.csv(file = 'Article_world_201912_202001.csv', header = F, encoding = 'UTF-8')

# delColArticle생성 (섹션, 주제, 원문)
delColArticle = select(article, -c(1, 3, 6))

delColArticle[1, c(2, 3)] # 주제포함 원문

# 명사 추출
library(KoNLP)
library(tm)
useNIADic()

# 문자 변환 -> 명사 추출 -> 공백으로 합침
exNoun = function(x) {
  paste(extractNoun(as.character(x)), collapse = " ")
}

nouns
sapply(strsplit(nouns, " "), length) #nouns의 개수
library(stringr)
word(nouns, 1)
# TF-IDF 기반
# Transaction 전처리
# 빈 list 생성
articleWordlistTF = list()
#:nrow(article)
for(i in 1:nrow(article)){ #1:48 시작 
  print(as.character(paste(i/nrow(article) * 100, "%", sep = " ")))
  a = as.character(delColArticle[i, 2])
  b = as.character(delColArticle[i, 3])
  nouns = sapply(paste(a, b, sep = " "), exNoun)
  # nouns의 단어 loop
  for(j in 1:sapply(strsplit(nouns, " "), length)){
    inTheList = F
    # tf-idf Ranking 15% = 1653 10% = 1102
    for(k in 1:1102){ # 550.95의 값은 전체 word의 5%
      # 만약 nouns내에 있는 단어가 tf-idf단어가 아니라면 
      # ""로 바꿔줌
      # 아니면, 그대로 내비둠.
      if(grepl(rownames(score_tfidf)[k], word(nouns, j), fixed = T)){
        nouns = gsub(word(nouns, j), rownames(score_tfidf)[k], nouns) # 불필요한 .이나 세모 같은거 지우려고 넣음
        inTheList = T
        # print(paste("tfidf: ", rownames(score_tfidf)[k]))
        # print(paste("word : ", word(nouns, j)))
      }
    }
    if(!inTheList){
      nouns = gsub(word(nouns, j), "", nouns)
    }
  }
  # 기타 stopwords 제거
  nouns = gsub("연합뉴스", "", nouns)
  nouns = gsub("들이", "", nouns)
  nouns = gsub("하지", "", nouns)
  nouns = gsub("newsis", "", nouns)
  nouns = gsub("뉴시스", "", nouns)
  nouns = gsub("photo", "", nouns)
  # print(nouns)
  #nouns = sapply(delColArticle[i, c(2, 3)], exNoun)
  myCorpus = Corpus(VectorSource(nouns))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, tolower)
  myCorpus = tm_map(myCorpus, removeWords, stopwords('en'))
  myCorpus_txt = TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, Inf)))
  myTerm = as.data.frame(as.matrix(myCorpus_txt))
  print(as.character(paste("myTerm : ", myTerm, sep = " ")))
  if(grepl(as.character(myTerm), "numeric(0)", fixed = T)){ #결측값 제거
    print("0 word occured deleting trasection")
  } else {
    articleWordlistTF[i] = list(rownames(myTerm))
  }
}

articleWordlistTF
articleWordlistTFnaNull = list.clean(articleWordlistTF, fun = is.null)
length(articleWordlistTF)
length(articleWordlistTFnaNull)
# list.clean을 통해 null transection 제거
library(rlist)
list.clean(articleWordlistTF, fun = is.null)

articleWordlist = list()
# Non TF-IDF 기반
for(i in 1:nrow(article)){
  print(as.character(paste(i/nrow(article) * 100, "%", sep = " ")))
  a = as.character(delColArticle[i, 2])
  b = as.character(delColArticle[i, 3])
  nouns = sapply(paste(a, b, sep = " "), exNoun)

  # 기타 stopwords 제거
  nouns = gsub("연합뉴스", "", nouns)
  nouns = gsub("들이", "", nouns)
  nouns = gsub("하지", "", nouns)
  nouns = gsub("newsis", "", nouns)
  nouns = gsub("뉴시스", "", nouns)
  # print(nouns)
  #nouns = sapply(delColArticle[i, c(2, 3)], exNoun)
  myCorpus = Corpus(VectorSource(nouns))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, tolower)
  myCorpus = tm_map(myCorpus, removeWords, stopwords('en'))
  myCorpus_txt = TermDocumentMatrix(myCorpus, control = list(wordLengths=c(4, Inf)))
  myTerm = as.data.frame(as.matrix(myCorpus_txt))
  articleWordlist[i] = list(rownames(myTerm))
}
articleWordlist[2]

for(i in 1:length(articleWordlist)){
  gsub("미국", "", articleWordlist[i])
}

# 트랜젝션 생성 
library(arules)

## set transaction names
names(articleWordlistTF) <- paste("Tr",c(1:1700), sep = "")
names(articleWordlistTFnaNull) <- paste("Tr",c(1:1432), sep = "")
names(articleWordlist) <- paste("Tr",c(1:1700), sep = "")

?as
## transaction 생성 
wordTrans = as(articleWordlistTF, "transactions")
wordTrans = as(articleWordlistTFnaNull, "transactions")
wordTrans = as(articleWordlist, "transactions")

inspect(wordTrans[1:100])

#frequent item header table 생성 
transrules = apriori(wordTrans, parameter = list(target = "frequent itemsets"))
transrules = apriori(wordTrans, parameter = list(supp = 0.01, conf = 1, maxlen = 100))

#minlen - minimum length of rules


inspect(transrules)
length(transrules)
summary(transrules)

rules = labels(transrules, ruleSep = " ")
rules = sapply(rules, strsplit, " ", USE.NAMES = F)
rules[1:100]
rulemat = do.call("rbind", rules)

library("igraph")

# via igraph only
ruleg = graph.edgelist(rulemat, directed = F)
ruleg

plot.igraph(as.directed(ruleg),
            edge.arrow.size = 0.1, 
            vertex.label = V(ruleg)$name,
            vertex.label.cex = 0.5, 
            vertex.label.color = 'black', 
            vertex.size = 5, # 글자 크기 
            vertex.color = 'white',
            vertex.frame.color = 'skyblue',
            edge.color = 'lightgray',
            margin = 0.01,
            )

# TF-IDF Ranking.
tf_allCorpus = NULL

for(i in 1:nrow(article)){
  print(as.character(paste(i/nrow(article) * 100, "%", sep = " ")), digits = 3)
  # 제목과 본문 합치기
  a = as.character(delColArticle[i, 2])
  b = as.character(delColArticle[i, 3])
  tf_nouns = sapply(paste(a, b, sep = " "), exNoun)
  tf_myCorpus = Corpus(VectorSource(tf_nouns))
  tf_myCorpus = tm_map(tf_myCorpus, removePunctuation)
  tf_myCorpus = tm_map(tf_myCorpus, removeNumbers)
  tf_myCorpus = tm_map(tf_myCorpus, tolower)
  tf_myCorpus = tm_map(tf_myCorpus, removeWords, stopwords('en'))

  if(is.null(tf_allCorpus)){
    tf_allCorpus = DocumentTermMatrix(tf_myCorpus, control = list(wordLengths=c(4, Inf)))
  } else {
    tf_tempCorpus = DocumentTermMatrix(tf_myCorpus, control = list(wordLengths=c(4, Inf)))
    tf_allCorpus = c(tf_allCorpus, tf_tempCorpus)
  }
}

dtm_tfidf = weightTfIdf(tf_allCorpus)
dtm_tfidf = removeSparseTerms(dtm_tfidf, as.numeric(0.999))
score_tfidf = colSums(as.matrix(dtm_tfidf)) # tf-idf가 계산된 dtm_tfidf를 matrix화함.
score_tfidf = as.data.frame(sort(score_tfidf, decreasing = T))
# tf-idf 상위 100개 문서 추출 
rownames(score_tfidf)[1:100]
(length(rownames(score_tfidf))/10)/2

