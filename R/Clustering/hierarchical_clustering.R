rm(list=ls())
# K-Means Clustering
setwd(paste("/Users/kevinegan/Documents/Udemy Courses/",
            "Machine Learning A-Z/Machine Learning A-Z (Codes and Datasets)/",
            "Part 4 - Clustering/Section 25 - Hierarchical Clustering/R", sep = ""))

# Import the dataset
dataset <- read.csv("mall.csv")
# Remove ID, gender, and age
dataset <- dataset[, 4:ncol(dataset)]

# Using the dendrogram to find the optimal number of clusters
dendrogram <- hclust(dist(dataset, method = 'euclidean'),
                     method = 'ward.D')
# Plot Dendrogram
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')

# Fit Hierarchical Clustering to data
set.seed(29)
h_clust <- hclust(dist(dataset, method = 'euclidean'),
                  method = 'ward.D')
y_hc <- cutree(h_clust,
               k = 5)
# Visualizing the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clusters of Customers",
         xlab = "Annual Income",
         ylab = "Spending Score")
