# Load libraries and read files
packages <- c("dplyr", "fpc", "cluster", 
              "factoextra", "dendextend", 
              "psych", "qgraph")
lapply(packages, library, character.only = TRUE)

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

###################################################################
#  Factor analysis
#  Useful tutorial: 
#  http://www.di.fc.ul.pt/~jpn/r/factoranalysis/factoranalysis.html
#  https://rpubs.com/aaronsc32/factor-analysis-introduction
###################################################################
# determined the number of factors to use with scree plot
parallel <- fa.parallel(scaled_data,
                        fm = 'minres',
                        fa = 'fa')

# factor analysis -- no rotation
# Varimax: assume factors completely uncorrelated
# Oblique: correlations in factors

# Method: factanal only support MaxLikelihood
# In fa (psych), we can use "PAF (pa)" or "mingres", 
# the later provide results similar to `MaxLikelihood` 
# without assuming multivariate normal distribution 
# and derives solutions through iterative eigen decomposition like principal axis.

fa1 <- factanal(scaled_data,
                factors=2, 
                rotation="none",
                scores="regression")

fa2 <- fa(scaled_data,
          nfactors = 3,
          rotate = "oblimin",
          fm="minres")
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


# we can get some flexibility from the "psych" package
fa_analysis <- function(data_set, factor,
                        rotate = "varimax", fm = "pa"){
  res <- fa(data_set, nfactors = factor,
            rotate = rotate, fm = fm)
  print("Factor Analysis results:")
  print(res)
  
  # get loading plot for the first two factors
  plot(res$loadings, pch=18, col='red')
  abline(h=0)
  abline(v=0)
  text(res$loadings, labels=names(data_set),cex=0.8)
  
  #get reproduced correlation matrix
  repro <- res$loadings%*%t(res$loadings)
  #residual correlation matrix
  residual <- cor(data_set)-repro
  print("Residual correlation matrx")
  round(resid2,2)
  
  #get root-mean squared residuals
  len <- length(residual[upper.tri(residual)])
  RMSR <- sqrt(sum(residual[upper.tri(residual)]^2)/len)
  print("Root-mean squared residuals:", RMSR)
  
  #get proportion of residuals greater than 0.05 in absolute value
  prop <- sum(rep(1,len)[abs(residual[upper.tri(residual)])>0.05])/len
  print("Proportion of residuals greater than 0.05 in absolute value:", prop)
}

# varimax - paf
fa_analysis(soccer, 3)

# quartimax - pag
fa_analysis(soccer, 3, "quartimax", "pa")
