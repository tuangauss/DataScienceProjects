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

source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")
soccer = read.csv("./Team2015season.csv",header=T)

rownames(soccer)=soccer[,1]

soccer <- soccer [,-1]
soccer <- soccer [, 1:20]

cor (soccer)

library(rela)
fact=paf(as.matrix(soccer))
summary(fact)
fact$KMO
bartlett.test(soccer)

#Deciding on the number of factors
compl=princomp(soccer, cor=T) #Remember cor=T so that PCA will use correlation matrix
print(summary(compl),digits=2, loadings=compl$loadings,cuttoff=0)

screeplot(compl, type="lines", col="red", lwd=2, pch=19, cex=1.2, main = "Scree Plot")
parallelplot(compl)

##########################################################
##  Perform Factor Analysis using Maximum Likelihood (only option in factanal)
##  with Varimax Rotation
##########################################################

#assuming 3 factors
fact1=factanal(soccer,factors=3,rotation="varimax")
fact1

#get loading plot for first two factors
plot(fact1$loadings, pch=18, col='red')
abline(h=0)
abline(v=0)
text(fact1$loadings, labels=names(soccer),cex=0.8)

#get reproduced correlation matrix
repro1=fact1$loadings%*%t(fact1$loadings)
#residual correlation matrix
resid1=fact1$cor-repro1
round(resid1,2)

#get root-mean squared residuals
len=length(resid1[upper.tri(resid1)])
RMSR1=sqrt(sum(resid1[upper.tri(resid1)]^2)/len)
RMSR1

#get proportion of residuals greater than 0.05 in absolute value
sum(rep(1,len)[abs(resid1[upper.tri(resid1)])>0.05])/len


##########################################################
##  Perform Factor Analysis using PAF
##  with Varimax Rotation
##########################################################
library(psych)
fact2=fa(soccer,nfactors=3,rotate="varimax", fm="pa")
fact2

#get loading plot for first two factors
plot(fact2$loadings, pch=18, col='red')
abline(h=0)
abline(v=0)
text(fact2$loadings, labels=names(soccer),cex=0.8)

#get reproduced correlation matrix
repro2=fact2$loadings%*%t(fact2$loadings)
#residual correlation matrix
resid2=cor(soccer)-repro2
round(resid2,2)

#get root-mean squared residuals
len=length(resid2[upper.tri(resid2)])
RMSR2=sqrt(sum(resid2[upper.tri(resid2)]^2)/len)
RMSR2

#get proportion of residuals greater than 0.05 in absolute value
sum(rep(1,len)[abs(resid2[upper.tri(resid2)])>0.05])/len


##########################################################
##  Perform Factor Analysis using PAF
##  with Quartimax Rotation
##########################################################

library(GPArotation)
fact3=fa(soccer,nfactors=3,rotate="quartimax", fm="pa")
fact3

#get loading plot for first two factors
plot(fact3$loadings, pch=18, col='red')
abline(h=0)
abline(v=0)
text(fact3$loadings, labels=names(soccer),cex=0.8)

#get reproduced correlation matrix
repro3=fact3$loadings%*%t(fact3$loadings)
#residual correlation matrix
resid3=cor(soccer)-repro3
round(resid3,2)

#get root-mean squared residuals
len=length(resid3[upper.tri(resid3)])
RMSR3=sqrt(sum(resid3[upper.tri(resid3)]^2)/len)
RMSR3

#get proportion of residuals greater than 0.05 in absolute value
sum(rep(1,len)[abs(resid3[upper.tri(resid3)])>0.05])/len

##########################################################
##  Perform Factor Analysis using Maximum Likelihood (only option in factanal)
##  with Quartimax Rotation
##########################################################

#He already presumes that there are two factors. I may need to 
#1) USE PCA to decide the number of factors
#2) Either use: a) PCA again to extract communalities or b) use other methods
fact4=factanal(soccer,factors=3,rotation="quartimax")
fact4

#get loading plot for first two factors
plot(fact4$loadings, pch=18, col='red')
abline(h=0)
abline(v=0)
text(fact4$loadings, labels=names(soccer),cex=0.8)

#get reproduced correlation matrix
repro4=fact4$loadings%*%t(fact4$loadings)
#residual correlation matrix
resid4=fact4$cor-repro4
round(resid4,2)

#get root-mean squared residuals
len=length(resid4[upper.tri(resid4)])
RMSR4=sqrt(sum(resid4[upper.tri(resid4)]^2)/len)
RMSR4

#get proportion of residuals greater than 0.05 in absolute value
sum(rep(1,len)[abs(resid4[upper.tri(resid4)])>0.05])/len
