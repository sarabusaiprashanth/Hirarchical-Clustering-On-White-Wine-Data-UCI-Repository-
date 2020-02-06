white.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
white.raw <- read.csv(white.url, header = TRUE, sep = ";")
View(white.raw)

white_wine <- white.raw
summary(white_wine)
colnames(white_wine)
white_wine <- white_wine[,-12] 
colnames(white_wine) #quality variable is removed
summary(white_wine)

?hclust
?scale


#normalization of the data
#white_wine_norm <- scale(white_wine)

#d <- dist(white_wine_norm, method = "euclidean")

#H.fit <- hclust(d, method = "complete")

#plot(H.fit) # display dendogram
#groups <- cutree(H.fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
#rect.hclust(H.fit, k=5, border="red") 


#d1 <- dist(white_wine_norm, method = "euclidean")
#H.fit1 <- hclust(d, method = "single") 
#plot(H.fit1)


#groups1 <- cutree(H.fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
#rect.hclust(H.fit1, k=5, border="red") 

#library(tidyverse)
#library(ggplot2)
#install.packages('dendextend', dependencies = TRUE)



#alt approach
whiteWINE <- as.data.frame(scale(white_wine))
summary(whiteWINE)
View(whiteWINE)


dist_mat <- dist(whiteWINE, method = "euclidean")
clust_model <- hclust(dist_mat, method = "complete") #complete linkage


cut_tree <- cutree(clust_model, k = 5)
cut_tree
plot(clust_model)
rect.hclust(clust_model, k = 5, border = 2:6)

#To know distance value of the the penultimate clusters merged we need
#to check the height of the tree
abline(h = 3, col = 'red')

dist_mat1 <- dist(whiteWINE, method = "euclidean")
clust_model1 <- hclust(dist_mat1, method = "single") #single linkage



library(factoextra)
library(cluster)


#Elbow curve to find optimum number of clusters
fviz_nbclust(whiteWINE, FUNcluster = hcut, method = "wss")

#silhouete method
fviz_nbclust(whiteWINE, FUNcluster = hcut, method = "silhouette")



#summary stats

place <- as.matrix(cut_tree)  

finalgorup <- data.frame(whiteWINE, place)

View(finalgorup)

#install.packages("clusterSim")
library(clusterSim)



#cluster.Description(whiteWINE, finalgorup, sdType="sample",precission=4,modeAggregationChar=";")


#data(finalgorup)

table(cut_tree)

finalgorup$volatile.acidity[cut_tree == 1]
finalgorup$volatile.acidity[cut_tree == 2]
finalgorup$volatile.acidity[cut_tree == 3]
finalgorup$volatile.acidity[cut_tree == 4]
finalgorup$volatile.acidity[cut_tree == 5]



sapply(unique(cut_tree), function(g)finalgorup$volatile.acidity[cut_tree == g])

median <- aggregate(finalgorup, list(cut_tree), median) #median
median

mean <- aggregate(finalgorup, list(cut_tree), mean)
mean


mode <- aggregate(finalgorup, list(cut_tree), mode)
mode




























