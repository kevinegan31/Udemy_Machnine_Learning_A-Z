# Data Preprocessing

# Set working directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/",
            "Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 -",
            " Data Preprocessing --------------------/R", sep = ""))
# Import the dataset
dataset <- read.csv("Data.csv")

# Taking care of missing data
dataset$Age <- ifelse(
  is.na(dataset$Age),
  yes = ave(
    dataset$Age,
    FUN = function(x) {
      mean(x, na.rm = TRUE)
    }
  ),
  no = dataset$Age
)

dataset$Salary <- ifelse(
  is.na(dataset$Salary),
  yes = ave(
    dataset$Salary,
    FUN = function(x) {
      mean(x, na.rm = TRUE)
    }
  ),
  no = dataset$Salary
)

any(is.na(dataset))

# Encoding categorical data

# Use factor function to transform categorical to numeric categories
# Factor also provides factors for each column
dataset$Country <- factor(
  dataset$Country,
  levels = unique(dataset$Country), # provides levels for each country
  labels = seq_along(unique(dataset$Country)) # labels numerically based on vector
)

dataset$Purchased <- factor(
  dataset$Purchased,
  levels = unique(dataset$Purchased),
  labels = seq_along(unique(dataset$Purchased))
)

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
