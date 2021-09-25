### k-Nearest Neighbour Classification
### Support Vector Machines
### Naive Bayes Classifier
### https://ithelp.ithome.com.tw/articles/10187804

library(ggplot2)
library(class)
library(dplyr)
library(e1071)


# 1. 載入iris資料
data(iris)

# 2. 畫散佈圖
library(ggplot2)

#花萼長寬分佈
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))

#花瓣長寬分佈
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))

# 2. 切分訓練及測試樣本
#設定亂數種子
set.seed(1111)

#樣本筆數
n <- nrow(iris)

#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- iris[t_idx, ]
testdata <- iris[-t_idx, ]


### k-Nearest Neighbour Classification
# 1. 安裝並載入class套件
library(class)
library(dplyr)

# 2. (參數1)準備訓練樣本組答案
trainLabels <- traindata$Species

# 3. (參數2)(參數3)去除兩個樣本組答案
knnTrain <- traindata[, -c(5)]
knnTest <- testdata[, -c(5)]

# 4. 計算k值(幾個鄰居)通常可以用資料數的平方根
kv <- round(sqrt(n))
kv

# 5. 建立模型 
prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = kv)

# 6. 評估正確性
cm <- table(x = testdata$Species, y = prediction, dnn = c("實際", "預測"))
cm

knnaccuracy <- sum(diag(cm))/sum(cm)
knnaccuracy

##執行此段需重新設定knnTrain, knnTest
# 7. 利用knn結果畫圖
#訓練組
knnTrain$Species <- trainLabels
ggplot(knnTrain, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species)) + stat_density2d(aes(color = Species))

#測試組
knnTest$Species <- prediction
ggplot(knnTest, aes(x = Petal.Length, y = Petal.Width), n = 10) + geom_point(aes(color = Species)) + stat_density2d(aes(color = Species), h = 0.6)
##

# 8. 選擇k value
klist <- seq(1:(kv+kv))
knnFunction <- function(x, knnTrain, knnTest, trainLabels, testLabels){
    prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = x)
    cm <- table(x = testLabels, y = prediction)
    accuracy <- sum(diag(cm))/sum(cm)
}
accuracies <- sapply(klist, knnFunction, knnTrain = knnTrain, knnTest = knnTest, trainLabels = trainLabels, testLabels = testdata$Species)


# 9. k value與準確度視覺化
df <- data.frame(kv = klist, accuracy = accuracies)
ggplot(df, aes(x = kv, y = accuracy, label = kv, color = accuracy)) + geom_point(size = 5) + geom_text(vjust = 2)



### Support Vector Machines
# 1. 載入套件 
library(e1071)

# 2. 建立模型
svmM <- svm(Species ~ ., data = traindata, probability = TRUE)

# 3. 預測
results <- predict(svmM, testdata, probability = TRUE)

# 4. 評估
cm <- table(x = testdata$Species, y = results)
cm
SVMaccuracy <- sum(diag(cm))/sum(cm)
SVMaccuracy



### Naive Bayes Classifier
# 1. 
nbcm <- naiveBayes(Species ~ ., data = traindata)
results <- predict(nbcm, testdata)

# 2. 評估
cm <- table(x = testdata$Species, y = results)
cm
naiveBayesaccuracy <- sum(diag(cm))/sum(cm)
naiveBayesaccuracy


### Summary
df <- data.frame(perf = c(knnaccuracy, SVMaccuracy, naiveBayesaccuracy), name = c("KNN", "SVM", "naiveBay"));

ggplot(df, aes(x = name, y = perf, color = name, label = perf)) + geom_point(size = 5) + geom_text(vjust = 2)


