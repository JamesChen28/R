### k-means
### https://ithelp.ithome.com.tw/articles/10187906

kmiris <- iris[-5] #去除第5個資料行    
##分3群，nstart=10 defaut執行10次  收斂資料區 
km <- kmeans(kmiris, centers = 3, nstart = 10)

#跑分群之後畫分佈 
plot(formula = Petal.Length ~ Petal.Width, data = kmiris, col = km$cluster, main = "將鳶尾花做分群", xlab = "花瓣寬度", ylab = "花瓣長度")

ggplot(kmiris, aes(x = Petal.Length, y = Petal.Width)) +
	geom_point(aes(color = factor(km$cluster))) +
	stat_density2d(aes(color = factor(km$cluster)))


### 組內距離平方和WSS(Within Cluster Sum of Squares) 越小越好
### 組間距離平方和BSS(Between Cluster Sum of Squares) 越大越好
### 總離均差平方和TSS(Total Cluster Sum of Squares)

(WSS <- km$tot.withinss)
(BSS <- km$betweenss)
(TSS <- BSS + WSS)
(ratio <- WSS / TSS)


klist <- seq(1:10)
knnFunction <- function(x) {
    kms <- kmeans(kmiris, centers = x, nstart = 1)
    ratio <- kms$tot.withinss / (kms$tot.withinss + kms$betweenss)
}
ratios <- sapply(klist, knnFunction)

# k value與準確度視覺化
df <- data.frame(kv = klist, KMratio = ratios)

ggplot(df, aes(x = kv, y = KMratio, label = kv, color = KMratio)) +
geom_point(size = 5) + geom_text(vjust = 2)




