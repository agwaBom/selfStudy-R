install.packages("ppclust")
install.packages("factoextra")
install.packages("dplyr")
install.packages("cluster")
install.packages("fclust")
install.packages("psych")

#기존 데이터 삭제 R
cat("\014")
rm(list=ls(all=TRUE))

library(ppclust)
library(factoextra)
library(dplyr)
library(cluster)
library(fclust)
library(psych)

sessionInfo()

data("iris")
irisWOLabel=iris[,-5]
irisWOLabel
# Produce Scatterplot matrices
pairs(irisWOLabel, col=iris[,5])

# Compute correlation of x and y
# default is pearson correlation
cor(iris[,1:4])

# SPLOM, Histogram and Correlation for a data matrix
# 히스토그램과 스캐터플롯, 상관계수에 대한 정보를 한눈에 보여주기 위한 함수
pairs.panels(iris[,-5], method = "pearson")

# Fuzzy C-Means Clustering
# partitions a numeric data set by using fuzzy c-means clustering algorithm
irisFuzzyClustering = fcm(irisWOLabel, centers=3)

# This shows dataset belong to which clusters in percentile in matrices
irisFuzzyClustering$u

# Matrices which shows cluster by independent variables
irisFuzzyClustering$v

summary(irisFuzzyClustering)

# Plots clustering results from a cluster analysis with ppclust
# objx - an object of ppclust class
# mt - dont need to know
# cm - specify the crisping method. default is max which is maximum degree of membership for each data object
#      but u can use "threshold" and type specific threshold number in tv between 0-1
# tv - use it if cm == "threshold"
# cp - integer for the index of avaliable color palettes 1(default)~5
plotcluster(irisFuzzyClustering, cp)


irisFuzzyClusteringViz = ppclust2(irisFuzzyClustering, "kmeans")
irisFuzzyClusteringViz
fviz_cluster(irisFuzzyClusteringViz, 
             data = irisWOLabel, 
             ellipse.type = "convex", 
             palette = "jco", 
             repel = TRUE)
