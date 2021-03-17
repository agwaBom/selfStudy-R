#고객 데이터

#data
id<-1:10
x<-c(2,20,20,5,12,4,15,27,25,16)
y<-c(20,18,5,28,24,15,5,10,4,12)
data<-data.frame(ID=id, 식료품=x, 의류=y)
data

#데이터 분포파악
plot(data$식료품, data$의류, xlab="식료품", ylab="의류",xlim=c(0,30), ylim=c(0,30))
text(data$식료품, data$의류, labels=data$ID, pos=4, col="blue")

#데이터 편차 표준화
data.scaled<-as.data.frame(scale(data[,-1], center=TRUE, scale=TRUE))
data.scaled 

#데이터 분포파악
plot(data.scaled$식료품, data.scaled$의류, xlab="식료품", ylab="의류",xlim=c(-2,2), ylim=c(-2,2))
text(data.scaled$식료품, data.scaled$의류, labels=data$ID, pos=4, col="blue")

#clustering
k<-2
kc<-kmeans(data.scaled, centers=k)
kc

#결과 분석
twss<-NULL
for(i in 1:9){
  kc<-kmeans(data.scaled, centers = i) #1..9개의 군집
  twss<-c(twss, kc$tot.withinss) #각 군집의 TWSS
}

plot(1:9, twss, xlim=c(0,10), type="b", xlab = "군집 수", ylab="TWSS")


#IRIS

#데이터 표준화
iris
data.scaled<-as.data.frame(scale(iris[,-5],center=TRUE, scale=TRUE))
head(data.scaled)

#clustering
kc<-kmeans(data.scaled,3)
kc

par(mar=c(5.1,4.1,4.1,7))
?park
kc$cluster
kc
plot(data.scaled[,1], data.scaled[,2],xlab="꽃받침 길이", ylab="꽃받침 너비",pch=21, col=kc$cluster)
legend("topright", legend=levels(iris$Species), pch=21, col=kc$cluster, xpd=TRUE, inset=c(-0.5,0))
points(kc$centers, pch=19, cex=1.5, col=rownames(kc$centers))
data.scaled

#정확도
table(kc$cluster, iris[,5])

twss<-NULL
for(i in 1:15){
  kc<-kmeans(data.scaled,centers=i)
  twss<-c(twss, kc$tot.withinss)
}
plot(1:15, twss, xlim=c(0,15), type="b", xlab="군집수", ylab="TWSS")

