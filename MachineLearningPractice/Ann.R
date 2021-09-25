### Artificial neural network
### https://ithelp.ithome.com.tw/articles/10187683


# 1. ���J�M��θ��
#�qgithub�U���M�� #plot.nnet
source("https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r")
library(DMwR)
library(nnet)
library(reshape)
library(devtools)
library(scales)
library(ggplot2)
library(neuralnet)
library(caret)

#�d�Ҩϥ�irisdata 
data(iris)

# 2. �����V�m�թM���ղո�ƶ�
set.seed(1117)

#���o�`����
n <- nrow(iris)

#�]�w�V�m�˥���70%
t_size = round(0.7 * n)

#���X�˥��ƪ�idx
t_idx <- sample(seq_len(n), size = t_size)

#�V�m�ռ˥�
traindata <- iris[t_idx, ]

#���ղռ˥�
testdata <- iris[-t_idx, ]

nnetM <- nnet(formula = Species ~ ., linout = T, size = 3, decay = 0.001, maxit = 1000, trace = T, data = traindata)

# 3. �e�� 
plot.nnet(nnetM, wts.only = F)


# 4. �w��
#test�հ���w�� 
prediction <- predict(nnetM, testdata, type = 'class')

#�w�����G 
cm <- table(x = testdata$Species, y = prediction, dnn = c("���", "�w��"))
cm


# 5. hidden layer = 6
nnetM <- nnet(formula = Species ~ ., linout = T, size = 6, decay = 0.001, maxit = 1000, trace = T, data = traindata)
plot.nnet(nnetM, wts.only = F)


### �h�����üh�������g�����M��
#���J�M��
library(neuralnet)

#��z���
datairis <- iris
datairis$setosa <- ifelse(datairis$Species == "setosa", 1, 0)
datairis$versicolor <- ifelse(datairis$Species == "versicolor", 1, 0)
datairis$virginica <- ifelse(datairis$Species == "virginica", 1, 0)

#�V�m�ҫ�
f1 <- as.formula("setosa + versicolor + virginica  ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
bpn <- neuralnet(formula = f1, data = datairis, hidden = c(2,4), learningrate = 0.01)
print(bpn)

#�ϸ�BP
plot(bpn)



###�ծհѼ�
library(caret)

#�w���̨ί��g���ѼƲզX
model <- train(form = f1, data = datairis, method = "neuralnet", tuneGrid = expand.grid(.layer1 = c(1:4), .layer2 = c(1:4), .layer3 = c(0)), learningrate = 0.01)
model
plot(model)






