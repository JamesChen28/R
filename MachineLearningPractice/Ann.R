### Artificial neural network
### https://ithelp.ithome.com.tw/articles/10187683


# 1. 載入套件及資料
#從github下載套件 #plot.nnet
source("https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r")
library(DMwR)
library(nnet)
library(reshape)
library(devtools)
library(scales)
library(ggplot2)
library(neuralnet)
library(caret)

#範例使用irisdata 
data(iris)

# 2. 分為訓練組和測試組資料集
set.seed(1117)

#取得總筆數
n <- nrow(iris)

#設定訓練樣本數70%
t_size = round(0.7 * n)

#取出樣本數的idx
t_idx <- sample(seq_len(n), size = t_size)

#訓練組樣本
traindata <- iris[t_idx, ]

#測試組樣本
testdata <- iris[-t_idx, ]

nnetM <- nnet(formula = Species ~ ., linout = T, size = 3, decay = 0.001, maxit = 1000, trace = T, data = traindata)

# 3. 畫圖 
plot.nnet(nnetM, wts.only = F)


# 4. 預測
#test組執行預測 
prediction <- predict(nnetM, testdata, type = 'class')

#預測結果 
cm <- table(x = testdata$Species, y = prediction, dnn = c("實際", "預測"))
cm


# 5. hidden layer = 6
nnetM <- nnet(formula = Species ~ ., linout = T, size = 6, decay = 0.001, maxit = 1000, trace = T, data = traindata)
plot.nnet(nnetM, wts.only = F)


### 多個隱藏層的類神經網路套件
#載入套件
library(neuralnet)

#整理資料
datairis <- iris
datairis$setosa <- ifelse(datairis$Species == "setosa", 1, 0)
datairis$versicolor <- ifelse(datairis$Species == "versicolor", 1, 0)
datairis$virginica <- ifelse(datairis$Species == "virginica", 1, 0)

#訓練模型
f1 <- as.formula("setosa + versicolor + virginica  ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
bpn <- neuralnet(formula = f1, data = datairis, hidden = c(2,4), learningrate = 0.01)
print(bpn)

#圖解BP
plot(bpn)



###調校參數
library(caret)

#預測最佳神經元參數組合
model <- train(form = f1, data = datairis, method = "neuralnet", tuneGrid = expand.grid(.layer1 = c(1:4), .layer2 = c(1:4), .layer3 = c(0)), learningrate = 0.01)
model
plot(model)







