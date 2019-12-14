# Load libraries and read files
packages <- c("dplyr", "fpc", "cluster", 
              "factoextra", "dendextend", 
              "psych", "qgraph")
lapply(packages, library, character.only = TRUE)
source("http://www.reuningscherer.net/STAT660/R/parallel.r.txt")

raw_df <- read.csv("./Team2015season.csv", header=T)
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

###########################################
#  K-means clustering
#  Useful tutorial: 
#  https://uc-r.github.io/kmeans_clustering
###########################################

# use 4 centers that Hc clustering suggests
# nstart: attempts multiple initial configurations
# and reports on the best one.
km_results <- kmeans(scaled_data, centers = 4, nstart = 100)
km_results

# fviz_cluster does PCA and plot the data points 
# according to the first two PCs that explain the majority of the variance
fviz_cluster(km_results, data = scaled_data)

# Evaluating clustering
# Best number of cluster using scree-plot (elbow method)
# optimal total-wihtin cluster sum of square
set.seed(123)
fviz_nbclust(scaled_data, kmeans, method = "wss")

# Average Silhouette method
# measuring the quality of the clusters
# by how well object lies within a cluster
# try to maximize average silhouette
fviz_nbclust(scaled_data, kmeans, method = "silhouette")

# GAP statistics method
# can apply to both kmeans and HC
# compares the total intracluster variation
# with their expected values 
# under null reference distribution of the data
# at various value of k
set.seed(123)
gap_stat <- clusGap(scaled_data,
                    FUN = kmeans,
                    nstart = 100,
                    K.max = 10,
                    B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

###########################################
#  Factor analysis
#  Useful tutorial: 
#  
###########################################

# factor analysis -- no rotation
fa1 <- factanal(scaled_data,
                factors=2, 
                rotation="none",
                scores="regression")
fa1
# biplot
biplot(fa1$scores[,1:2],
       loadings(fa1),
       cex=c(0.7,0.8))
# qgraph
# a different visualization of biplot
qg.fa1 <- qgraph(fa1)

# NOTE:
# - after Exploratory Factor Analysis (EFA), 
# - the next step could be Confirmatory Factor Analysis
# - which is part of a larger subset: Structual Equation Modelling 
# - https://socialsciences.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf

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
