### Decision tree
### Conditional Inference Tree
### Random forest
### https://ithelp.ithome.com.tw/articles/10187561

install.packages("party")
install.packages("AER")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("randomForest") 

library(party)
library(AER)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest) 


### Decision tree
library("rpart")
library("rpart.plot")
library("rattle")

# 1. 載入creditcard資料集(包含1,319筆觀察測試，共有12個變數)
data(CreditCard)
dim(CreditCard)
colnames(CreditCard)

#假設我們只要以下欄位(card:是否核准卡片、信用貶弱報告數、年齡、收入(美金)、自有住宅狀況、往來年月)
bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner, months))
#將是否核准卡片轉換為0/1數值
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0)


# 2. 測試模型
#取得總筆數
n <- nrow(bankcard)
#設定隨機數種子
set.seed(1117)
#將數據順序重新排列
newbankcard <- bankcard[sample(n), ]

#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- newbankcard[t_idx, ]
testdata <- newbankcard[-t_idx, ]


# 3. 建立決策樹模型 
dtreeM <- rpart(formula = card ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001))


# 4. 用rattle畫出厲害的決策樹(Rx: rxDTree)
fancyRpartPlot(dtreeM)


# 5. 預測
result_dt <- predict(dtreeM, newdata = testdata, type = "class")


# 6. 建立混淆矩陣(confusion matrix)觀察模型表現
cm_dt <- table(testdata$card, result_dt, dnn = c("實際", "預測"))
cm_dt

# 7. 正確率
#計算核準卡正確率
cm_dt[4]/sum(cm_dt[, 2])

#計算拒補件正確率
cm_dt[1]/sum(cm_dt[, 1])

#整體準確率(取出對角/總數)
accuracy_dt <- sum(diag(cm_dt))/sum(cm_dt)
accuracy_dt



### Conditional Inference Tree
library(party)

ct <- ctree(Species ~ ., data = iris)
plot(ct, main = "條件推論樹")
table(iris$Species, predict(ct))



### Random forest
library(randomForest)

# 1.
set.seed(1117)


# 2. 跑隨機樹森林模型
randomforestM <- randomForest(card ~ ., data = traindata, importane = T, proximity = T, do.trace = 100)
randomforestM


#錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(randomforestM)

#衡量每一個變數對Y值的重要性，取到小數點第二位
randomForest::importance(randomforestM)
	# round(importance(randomforestM), 2)

# 3. 預測
result_rf <- predict(randomforestM, newdata = testdata)
result_rf_Approved <- ifelse(result_rf > 0.6, 1, 0)


# 4. 建立混淆矩陣(confusion matrix)觀察模型表現
cm_rf <- table(testdata$card, result_rf_Approved, dnn = c("實際", "預測"))
cm_rf


# 5. 正確率
#計算核準卡正確率
cm_rf[4] / sum(cm_rf[, 2])

#計算拒補件正確率
cm_rf[1] / sum(cm_rf[, 1])

#整體準確率(取出對角/總數)
accuracy_rf <- sum(diag(cm_rf)) / sum(cm_rf)
accuracy_rf






