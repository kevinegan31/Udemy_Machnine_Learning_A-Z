rm(list = ls())
# Logistic Regression
setwd(paste("/Users/kevinegan/Documents/Udemy Courses/",
            "Machine Learning A-Z/Machine Learning A-Z (Codes and Datasets)/",
            "Part 3 - Classification/Section 14 - Logistic Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("Social_Network_Ads.csv")
# Remove user id and gender
dataset <- dataset[, 3:ncol(dataset)]
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

# Fit logistic regression model
log_reg <-
  glm(Purchased ~ .,
      family = binomial(link = "logit"),
      data = dataset_train)

# Predicting the test set results
# Vector of the predicted probabilities of the test set
# For example, this predicts that User #2 will have a low chance
# of buying an SUV
prob_pred <- predict(log_reg,
                     type = 'response',
                     newdata = dataset_test[-ncol(dataset_test)])
# Converting predicted probabilities to 1 or 0
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
# Get predictions of whether or not event occurs
# First customer of the test set
pred_1 <- predict(log_reg,
                  type = 'response',
                  newdata = dataset_test[1, 1:(ncol(dataset) - 1)])
ifelse(pred_1 > 0.5, 1, 0)

# Making the Confusion Matrix
cm <- table(dataset_test[, ncol(dataset_test)],
            y_pred)
cm

#Creating confusion matrix with caret
library(caret)
cm2 <- confusionMatrix(data=as.factor(y_pred),
                       reference = as.factor(dataset_test[, ncol(dataset_test)]))
cm2
# Visualizing the results from the training set
library(ElemStatLearn)
set = dataset_train
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(log_reg, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
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
prob_set = predict(log_reg, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))
