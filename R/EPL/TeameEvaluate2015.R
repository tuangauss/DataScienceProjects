library(fpc)
library(cluster)

###############################################################
#  Hierarchical Cluster Analysis - Soccer Data
###############################################################
raw_df <- read.csv("./Team2015season.csv",header=T)

#standardized data
data1 <- raw_df[,2:20]
rownames(data1) <- raw_df[,1]
data1<- scale (data1)

#Eucledian, Ward's method
dist1<-dist(data1, method="euclidean")
clust1<-hclust(dist1,method="ward.D")
#draw the dendrogram
plot(clust1,labels= raw_df$Team, cex=0.7, xlab="",ylab="Distance",main="Clustering for 60 teams")
rect.hclust(clust1,k=4)

#get membership vector 
cuts=cutree(clust1,k=4)
cuts


source("http://reuningscherer.net/stat660/R/HClusEval.R.txt")
hclus_eval(data1, dist_m = 'euclidean', clus_m = 'ward', plot_op = T)

#Make plot of four cluster solution in space desginated by first two
#  two discriminant functions

#Eucledian, single linkage method
dist2<-dist(data1, method="euclidean")
clust2<-hclust(dist2,method="single")
#draw the dendrogram
plot(clust2,labels=raw_df$Team, cex=0.7, xlab="",ylab="Distance",main="Clustering for 60 teams")
rect.hclust(clust2,k=3)

#get membership vector (which country is in which group)
cuts=cutree(clust1,k=5)
cuts

hclus_eval(data1, dist_m = 'euclidean', clus_m = 'single', plot_op = T)


#Manhattan, Ward's method
dist3<-dist(data1, method="manhattan")
clust3<-hclust(dist3,method="ward.D")
#draw the dendrogram
plot(clust3,labels= data$Team, cex=0.7, xlab="",ylab="Distance",main="Clustering for 60 teams")
rect.hclust(clust3,k=4)

#get membership vector (which country is in which group)
cuts=cutree(clust3,k=5)
cuts

hclus_eval(data1, dist_m = 'manhattan', clus_m = 'ward', plot_op = T)

#Manhattan, single linkage method
dist4<-dist(data1, method="manhattan")
clust4<-hclust(dist4,method="single")
#draw the dendrogram
plot(clust4,labels= data$Team, cex=0.7, xlab="",ylab="Distance",main="Clustering for 60 teams")
rect.hclust(clust3,k=3)

#get membership vector (which country is in which group)
cuts=cutree(clust3,k=3)
cuts

hclus_eval(data1, dist_m = 'manhattan', clus_m = 'single', plot_op = T)

###############################################################
#  K-means Clustering - Soccer Data
###############################################################

library(psych)

#Just try five clusters to see how this works
#  Centers gives the number of clusters desired.  
#  You can either give a vector with the original centers, OR just specify the number of clusters.
km1=kmeans(data1,centers=4)

#see which teams are in each cluster
for (i in 1:4){
  print(paste("Teams in Cluster ",i))
  print(data$Team[km1$cluster==i])
  print (" ")
}
