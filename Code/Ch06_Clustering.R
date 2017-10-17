## ----opts, echo = FALSE--------------------------------------------------
library(knitr)
opts_chunk$set(dev="png", 
               dev.args=list(type="cairo"),
               dpi=96)
knitr::opts_chunk$set(
  fig.path = "~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Images/004/"
)

## ----ch06_hierachal_clust_01---------------------------------------------
# Read the house Worth Data
Data_House_Worth <- read.csv("~/Dropbox/Book Writing - Drafts/Final Artwork and Code/Chapter 6/Dataset/House Worth Data.csv",header=TRUE);

str(Data_House_Worth)

#remove teh extra column as well not be using this
Data_House_Worth$BasementArea <- NULL

## ----ch06_hierachal_clust_02---------------------------------------------
library(ggplot2)
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + geom_point()

## ----ch06_hierachal_clust_03---------------------------------------------
# apply the hirarichal clustering algorith
clusters <- hclust(dist(Data_House_Worth[,2:3]))

#Plot the dendogram
plot(clusters)

## ----ch06_hierachal_clust_04---------------------------------------------
# Create different number of clusters
clusterCut_2 <- cutree(clusters, 2)
#table the clustering distribution with actual networth
table(clusterCut_2,Data_House_Worth$HouseNetWorth)

clusterCut_3 <- cutree(clusters, 3)
#table the clustering distribution with actual networth
table(clusterCut_3,Data_House_Worth$HouseNetWorth)


clusterCut_4 <- cutree(clusters, 4)
#table the clustering distribution with actual networth
table(clusterCut_4,Data_House_Worth$HouseNetWorth)

## ----ch06_hierachal_clust_05---------------------------------------------
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut_3) + 
  scale_color_manual(values = c('black', 'red', 'green'))

## ----ch06_kmeans_clust_06------------------------------------------------

# Elbow Curve

wss <- (nrow(Data_House_Worth)-1)*sum(apply(Data_House_Worth[,2:3],2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(Data_House_Worth[,2:3],centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

## ----ch06_kmeans_clust_07------------------------------------------------
set.seed(917)
#Run k-means cluster of the datase
Cluster_kmean <- kmeans(Data_House_Worth[,2:3], 3, nstart = 20)

#Tabulate teh cross distribution
table(Cluster_kmean$cluster,Data_House_Worth$HouseNetWorth)

## ----ch06_kmeans_clust_08------------------------------------------------
Cluster_kmean$cluster <- factor(Cluster_kmean$cluster)
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = Cluster_kmean$cluster) + 
  scale_color_manual(values = c('black', 'red', 'green'))

## ----ch06_dist_clust_01--------------------------------------------------
library(EMCluster, quietly = TRUE)

ret <- init.EM(Data_House_Worth[,2:3], nclass = 3)
ret

ret.new <- assign.class(Data_House_Worth[,2:3], ret, return.all = FALSE)

#This have assigned a class to each case
str(ret.new)

# Plot results
plotem(ret,Data_House_Worth[,2:3])

## ----ch06_disty_clust_02-------------------------------------------------
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = ret.new$class) + 
  scale_color_manual(values = c('black', 'red', 'green'))

## ----ch06_dbscan_01------------------------------------------------------
library(dbscan)
cluster_dbscan <- dbscan(Data_House_Worth[,2:3],eps=0.8,minPts = 10)
cluster_dbscan

#Display the hull plot
hullplot(Data_House_Worth[,2:3],cluster_dbscan$cluster)

## ----ch06_dunnIndex------------------------------------------------------
library(clValid)
#Showing for hieracical cluster with clusters = 3
dunn(dist(Data_House_Worth[,2:3]), clusterCut_3)

## ----ch06_Silhoutte------------------------------------------------------
library(cluster)

#Showing for k-means cluster with clusters = 3
sk <- silhouette(clusterCut_3,dist(Data_House_Worth[,2:3]))

plot(sk)

## ----ch06_rand-----------------------------------------------------------
#Unign result from EM Algo
library(EMCluster)
clust <- ret.new$class
orig <- ifelse(Data_House_Worth$HouseNetWorth == "High",2,
               ifelse(Data_House_Worth$HouseNetWorth == "Low",1,2))
RRand(orig, clust)

## ----ch06_jaccard--------------------------------------------------------
#Unign result from EM Algo
library(EMCluster)
clust <- ret.new$class
orig <- ifelse(Data_House_Worth$HouseNetWorth == "High",2,
               ifelse(Data_House_Worth$HouseNetWorth == "Low",1,2))
Jaccard.Index(orig, clust)

