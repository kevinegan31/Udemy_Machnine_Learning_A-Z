# Data Preprocessing

# Set working directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/",
            "Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 -",
            " Data Preprocessing --------------------/R", sep = ""))
# Import the dataset
dataset <- read.csv("Data.csv")

# Split into train/test set
# install.packages("caTools")
# caTools library helps split into training/test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)

# Training set
dataset_train <- subset(dataset, split == TRUE)
# Test set
dataset_test <- subset(dataset, split == FALSE)

# Feature Scaling
dataset_train[, 2:3] <- scale(dataset_train[, 2:3])
dataset_test[, 2:3] <- scale(dataset_test[, 2:3])
