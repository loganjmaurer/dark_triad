library(tidyverse)
library(ggcorrplot)
library(cluster)

#read data
df <- read.csv("data.csv", sep = "\t")
mach_items <- df %>% select(1:9) %>% scale()
narc_items <- df %>% select(10:18) %>% scale()
psych_items <- df %>% select(19:27) %>% scale()

#exploratory analysis
ggcorrplot(cor(mach_items))
ggcorrplot(cor(narc_items))
ggcorrplot(cor(psych_items))

#PCA
pca <- prcomp(df[, 1:27], scale. = TRUE)
X <- pca$x

#clustering
kmeans_model <- kmeans(df[, 1:27], centers = 3)

# Visualize the clustering results using ggplot2
ggplot(as.data.frame(X), aes(x = PC1, y = PC2, color = as.factor(kmeans_model$cluster))) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", 
       color = "Cluster") +
  ggtitle("K-Means Clustering on Principal Components")