# Load libraries and read files
packages <- c("dplyr", "fpc", "cluster", "psych", "dendextend")
lapply(packages, library, character.only = TRUE)
source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")

raw_df <- read.csv("./Team2015season.csv",header=T)
# scale data 

scaled_data <- raw_df %>%
  remove_rownames() %>%
  column_to_rownames("Team") %>%
  scale()


#######################################
#  Hierarchical Cluster Analysis
#  Useful tutorial: 
#  https://uc-r.github.io/hc_clustering
#######################################

#Eucledian, Ward's method
d_1 <- dist(scaled_data, method="euclidean")
clust_1 <- hclust(d_1, method="ward.D")
#draw the dendrogram
plot(clust_1,
     cex=0.7,
     xlab="",
     ylab="Distance",
     main="Clusterings of 60 European teams")
rect.hclust(clust_1, k = 4, border = 2:5)

#get membership vector 
cuts <- cutree(clust_1,k=4)
scaled_data %>%
  as.data.frame() %>%
  mutate(cluster = cuts) %>%
  head

# Compute distance matrix
res.dist <- dist(scaled_data, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms and compare group partition
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           lwd = 1,
           edge.lwd = 1,
           lab.cex = 0.5,
           columns_width = c(8, 3, 8),
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

#######################################
#  K-means clustering
#  Useful tutorial: 
#  https://uc-r.github.io/hc_clustering
#######################################

# use 4 centers that Hc clustering suggests
# nstart: attempts multiple initial configurations
# and reports on the best one.
km_results <- kmeans(scaled_data, centers = 4, nstart = 100)
km_results

# fviz_cluster does PCA and plot the data points 
# according to the first two PCs that explain the majority of the variance
fviz_cluster(km_results, data = scaled_data)


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
