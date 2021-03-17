### Principal Component Analysis
getwd()
setwd("/Users/hyunjinkim/dev/Rproject/SVD_PCA")

finance = read.csv("./secu_com_finance_2007_c.csv", encoding = "UTF-8", header = T, stringsAsFactors = FALSE)


## 표준화 변환(Standardization)
# use scale function and add column V1_s ~ V5_s which is standardization of V1 ~ V5
finance = transform(finance,
                    V1_s = scale(V1),
                    V2_s = scale(V2),
                    V3_s = scale(V3),
                    V4_s = scale(V4),
                    V5_s = scale(V5))

# add new column V4_s2 which is subtracted observation value after normalized with max Value
# because the more 부채비율(V4)is, it means worse
finance = transform(finance, V4_s2 = max(V4_s) - V4_s)

# variable selection (원하는 variable column 값만 고름)
finance_1 = finance[, c("company", "V1_s", "V2_s", "V3_s", "V4_s2", "V5_s")]
finance_1

# correlation analysis
cor(finance_1[,-1])

# Scatter plot matrix
plot(finance_1[,-1])

# Principal Component Analysis
# use 2 ~ 6 column of finance_1 data
finance_1_PCA = prcomp(finance_1[,c(2:6)])

# Can look PC1~PC1&PC2 on Cumulative proportion take most
summary(finance_1_PCA)

finance_1_PCA

## how many PCs should we choose?
## rule of thumb
# 1. Cumulative proportion should be used at least 0.8
# 2. PCs should be more than mean & variance
# 3. if Scree plot is elbowed(꺾임), use PC number before elbow

# Scree plot
# elbow is created at 4, so use 3 PCs(주성분)
plot(finance_1_PCA, type="l", sub="Scree Plot")

biplot(finance_1_PCA, cex=c(0.7, 0.8))

# calculate pc1 and pc2 score
pc1 = predict(finance_1_PCA)[,1]
pc2 = predict(finance_1_PCA)[,2]
text(pc1, pc2, labels = finance_1$company, cex = 0.7, pos = 3, col = "blue")
