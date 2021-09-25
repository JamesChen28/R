### k-Nearest Neighbour Classification
### Support Vector Machines
### Naive Bayes Classifier
### https://ithelp.ithome.com.tw/articles/10187804

library(ggplot2)
library(class)
library(dplyr)
library(e1071)


# 1. ���Jiris���
data(iris)

# 2. �e���G��
library(ggplot2)

#�Ḱ���e���G
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(aes(color = Species))

#��ä���e���G
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species))

# 2. �����V�m�δ��ռ˥�
#�]�w�üƺؤl
set.seed(1111)

#�˥�����
n <- nrow(iris)

#���X�˥��ƪ�idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#�V�m��ƻP���ո�Ƥ��: 70%�ؼҡA30%����
traindata <- iris[t_idx, ]
testdata <- iris[-t_idx, ]


### k-Nearest Neighbour Classification
# 1. �w�˨ø��Jclass�M��
library(class)
library(dplyr)

# 2. (�Ѽ�1)�ǳưV�m�˥��յ���
trainLabels <- traindata$Species

# 3. (�Ѽ�2)(�Ѽ�3)�h����Ӽ˥��յ���
knnTrain <- traindata[, -c(5)]
knnTest <- testdata[, -c(5)]

# 4. �p��k��(�X�ӾF�~)�q�`�i�H�θ�Ƽƪ������
kv <- round(sqrt(n))
kv

# 5. �إ߼ҫ� 
prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = kv)

# 6. �������T��
cm <- table(x = testdata$Species, y = prediction, dnn = c("���", "�w��"))
cm

knnaccuracy <- sum(diag(cm))/sum(cm)
knnaccuracy

##���榹�q�ݭ��s�]�wknnTrain, knnTest
# 7. �Q��knn���G�e��
#�V�m��
knnTrain$Species <- trainLabels
ggplot(knnTrain, aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species)) + stat_density2d(aes(color = Species))

#���ղ�
knnTest$Species <- prediction
ggplot(knnTest, aes(x = Petal.Length, y = Petal.Width), n = 10) + geom_point(aes(color = Species)) + stat_density2d(aes(color = Species), h = 0.6)
##

# 8. ���k value
klist <- seq(1:(kv+kv))
knnFunction <- function(x, knnTrain, knnTest, trainLabels, testLabels){
    prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = x)
    cm <- table(x = testLabels, y = prediction)
    accuracy <- sum(diag(cm))/sum(cm)
}
accuracies <- sapply(klist, knnFunction, knnTrain = knnTrain, knnTest = knnTest, trainLabels = trainLabels, testLabels = testdata$Species)


# 9. k value�P�ǽT�׵�ı��
df <- data.frame(kv = klist, accuracy = accuracies)
ggplot(df, aes(x = kv, y = accuracy, label = kv, color = accuracy)) + geom_point(size = 5) + geom_text(vjust = 2)



### Support Vector Machines
# 1. ���J�M�� 
library(e1071)

# 2. �إ߼ҫ�
svmM <- svm(Species ~ ., data = traindata, probability = TRUE)

# 3. �w��
results <- predict(svmM, testdata, probability = TRUE)

# 4. ����
cm <- table(x = testdata$Species, y = results)
cm
SVMaccuracy <- sum(diag(cm))/sum(cm)
SVMaccuracy



### Naive Bayes Classifier
# 1. 
nbcm <- naiveBayes(Species ~ ., data = traindata)
results <- predict(nbcm, testdata)

# 2. ����
cm <- table(x = testdata$Species, y = results)
cm
naiveBayesaccuracy <- sum(diag(cm))/sum(cm)
naiveBayesaccuracy


### Summary
df <- data.frame(perf = c(knnaccuracy, SVMaccuracy, naiveBayesaccuracy), name = c("KNN", "SVM", "naiveBay"));

ggplot(df, aes(x = name, y = perf, color = name, label = perf)) + geom_point(size = 5) + geom_text(vjust = 2)

