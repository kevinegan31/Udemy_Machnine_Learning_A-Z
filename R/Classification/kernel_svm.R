rm(list = ls())
# Kernel SVM

setwd(paste("/Users/kevinegan/Documents/Udemy Courses/",
            "Machine Learning A-Z/Machine Learning A-Z (Codes and Datasets)/",
            "Part 3 - Classification/Section 17 - Kernel SVM/R", sep = ""))

# Import the dataset
dataset <- read.csv("Social_Network_Ads.csv")
# Remove user id and gender
dataset <- dataset[, 3:ncol(dataset)]

# Encoding the target feature as factor
dataset$Purchased <- factor(dataset$Purchased,
                            levels = c(0, 1))

# Split into train/test set
# install.packages("caTools")
# caTools library helps split into training/test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
# Training set
dataset_train <- subset(dataset, split == TRUE)
# Test set
dataset_test <- subset(dataset, split == FALSE)

# Feature Scaling
# Scale independent variables
dataset_train[, 1:(ncol(dataset) - 1)] <-
  scale(dataset_train[, 1:(ncol(dataset) - 1)])
dataset_test[, 1:(ncol(dataset) - 1)] <-
  scale(dataset_test[, 1:(ncol(dataset) - 1)])

# Fit classifier regression model
library(e1071)
classifier <- svm(formula = Purchased ~ .,
                  data = dataset_train,
                  type = 'C-classification',
                  kernel = 'radial')

# Predicting the test set results
# Vector of the predicted probabilities of the test set
# For example, this predicts that User #2 will have a low chance
# of buying an SUV
y_pred <- predict(classifier,
                  newdata = dataset_test[-ncol(dataset_test)])

# Making the Confusion Matrix
cm <- table(dataset_test[, ncol(dataset_test)],
            y_pred)
cm

#Creating confusion matrix with caret
library(caret)
cm2 <- confusionMatrix(cm)
cm2
# Visualizing the results from the training set
library(ElemStatLearn)
set = dataset_train
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier,
                 newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualizing the test set results
set = dataset_test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier,
                 newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

