rm(list=ls())
# K-Means Clustering
setwd(paste("/Users/kevinegan/Documents/Udemy Courses/",
            "Machine Learning A-Z/Machine Learning A-Z (Codes and Datasets)/",
            "Part 4 - Clustering/Section 24 - K-Means Clustering/R", sep = ""))

# Import the dataset
dataset <- read.csv("mall.csv")
# Remove ID, gender, and age
dataset <- dataset[, 4:ncol(dataset)]

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss <- vector()
k_clusters <- 10
for (i in seq_len(k_clusters)) {
  wcss[i] <- sum(kmeans(dataset, i)$withinss)
}
# Plot clusters
plot(
  seq_len(k_clusters),
  wcss,
  type = 'b',
  main = paste('Clusters of Customers'),
  xlab = "Number of clusters",
  ylab = "WCSS"
)
# Applying k-means to the mall dataset
set.seed(29)
k_means <- kmeans(dataset,
                  centers = 5,
                  iter.max = 300,
                  nstart = 10)

# Visualizing the clusters
library(cluster)
clusplot(dataset,
         k_means$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clusters of Customers",
         xlab = "Annual Income",
         ylab = "Spending Score")
