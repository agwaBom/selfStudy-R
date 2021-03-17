install.packages("arules")

library(arules)

# single
# "trans1 item1", 
# "trans2 item1",
# "trans2 item2",
trans = read.transactions(file.choose(), format="basket", sep=",")
getwd()
setwd("/Users/hyunjinkim/dev/Rproject/Apriori")
trans = read.transactions("sales.csv", format="basket", sep=",")

inspect(trans)

# 거래 건수
itemFrequency(trans, type="absolute")

# 거래 비율(해당 item이 있는 transaction/총 transaction)
itemFrequency(trans, type="relative")

# 아이템별 거래 빈도 차트(거래 건수)
itemFrequencyPlot(trans, type="absolute", xlab="item", ylab="count", col=1:5)
# 아이템별 거래 빈도 차트(비율)
itemFrequencyPlot(trans, type="relative", xlab="item", ylab="ratio", col=1:5)

# 연관규칙 생성
# Parameter specification
# minval - a numeric value for the minimal value of additional evaluation measure selected with arem (default: 0.1)
# smax - a numeric value for the maximal support of itemsets/rules/hyperedgesets (default: 1)
# arem - a character string indicating the used additional rule evaluation measure (default: "none") given by one of
#   none, diff, quot, aimp, info, chi2
# aval - a logical indicating whether to return the additional rule evaluation measure selected with arem.
# originalSupport - a logical indicating whether to use the original definition of minimum support (support of the LHS and RHS of the rule). If set to FALSE then a minimum threshold on coverage (i.e., the support of the LHS) is used instead. (default: TRUE)
# maxtime - Time limit in seconds for checking subsets. maxtime = 0 disables the time limit. (default: 5 seconds)
# ext - a logical indicating whether to report coverage (i.e., LHS-support) as an extended quality measure (default: TRUE)

# Algorithmic control
# filter - a numeric scalar indicating how to filter unused items from transactions (default: 0.1)
# tree - a logical indicating whether to organize transactions as a prefix tree (default: TRUE)
# heap - a logical indicating whether to use heapsort instead of quicksort to sort the transactions (default: TRUE)
# memopt - a logical indicating whether to minimize memory usage instead of maximize speed (default: FALSE)
# load - a logical indicating whether to load transactions into memory (default: TRUE)
# sort - an integer scalar indicating how to sort items with respect to their frequency: (default: 2(decending, ascending))
# verbose - a logical indicating whether to report progress

rules = apriori(trans, parameter = list(supp=0.4, conf=0.7, minlen=2))

# rule 개수
rules
# rules 출력
# coverage는 lhs아이템의 support를 나타냄(lhs가 있는 transaction/전체 transaction)
inspect(sort(rules, by="count", decreasing = TRUE))

# lift가 1 이상인 경우만 출력
rules2 = subset(rules, lift>1.0)
inspect(rules2)

# lift 1이상인 것을 내림차순 정렬
rules3 = sort(rules2, by="lift", decreasing = TRUE)
inspect(rules3)

install.packages("arulesViz")
library(arulesViz)

plot(rules2, method="graph", control=list(nodeCol="green", edgeCol="red", alpha=1))

inspect(trans)
# lhs는 A가 나와야 하는데 rhs는 건드리지 않을꺼니까 default에 둠
# minlen은 lhs와 rhs를 합한 요소의 개수 중, 최소한 충족해야할 요소의 개수.
rules4 = apriori(trans, parameter = list(supp=0.4, conf=0.7, minlen=2), 
                 appearance = list(lhs="A", default="rhs"))
inspect(rules4)

rules5 = apriori(trans, parameter = list(supp=0.4, conf=0.7, minlen=2), 
                 appearance = list(rhs="B", default="lhs"))
inspect(rules5)

# lift > 1.0인 것만 출력
rules6 = subset(rules5, lift>1.0)
inspect(rules6)


#####Groceries 데이터######

data(Groceries)
# density = non-zero element in matrix / 9835(row) * 169(columns)
# size - 1 Transaction에 몇 개를 샀는지
summary(Groceries)

# Transaction데이터 보기
inspect(Groceries)
inspect(Groceries[1:3])

# item별 거래 빈도(비율)
itemFrequency(Groceries)
itemFrequencyPlot(Groceries, xlab="상품 아이템", ylab="비율", col=1:5, topN=10)
sort(itemFrequency(Groceries) ,decreasing = TRUE)

rules = apriori(Groceries, parameter=list(supp=0.02, conf=0.4, minlen=2, maxlen=3))
inspect(rules)

# support에 따라 원 크기가 달라짐
# alpha는 투명도 0~1 설정 가능
# col을 설정하면 lift값에 따라 색이 바뀌지 않는 문제가 있다.
plot(rules, method = "graph", control=list(nodeCol="green", edgeCol="red", alpha=1))
plot(rules, method = "graph", control=list(nodeCol="green", edgeCol="red"))

# 해결책 1(원론적)
pal = c()
for(i in 1:15){
  pal = append(pal, paste("#00",as.hexmode(i),"000",sep=""))
}
pal
plot(rules, method = "graph", control = list(nodeCol=pal))

# 해결책 2(함수를 사용)
install.packages("grDevices") # 지금은 내장 패키지라 안깔아도 됨

pal = colorRampPalette(c("white","red"))
plot(rules, method = "graph", control = list(nodeCol=pal(100)))

# root vegetables에 영향을 받는 상품
rules2 = apriori(Groceries,
                 parameter = list(supp=0.02, conf=0.4, minlen=2, maxlen=3),
                 appearance = list(default="rhs", lhs="root vegetables"))
inspect(rules2)

# lhs || rhs에 특정 물품들만 찾아보고 싶다면?
rules2 = apriori(Groceries,
                 parameter = list(supp=0.02, conf=0.4, minlen=2, maxlen=3),
                 appearance = list(default="rhs", lhs=c("root vegetables", "whole milk")))
inspect(rules2)

# other vegetables에 영향을 주는 상품
rules3 = apriori(Groceries,
                 parameter = list(supp=0.02, conf=0.4, minlen=2, maxlen=3),
                 appearance = list(default="lhs", rhs="other vegetables"))
inspect(rules3)

#===========의사결정나무 분석을 통한 분류============
