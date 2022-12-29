rm(list = ls())
# Support Vector Regression

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 7 - Support Vector Regression (SVR)/R", sep = ""))

# Import the dataset
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[, 2:3] # subset

# Encoding categorical data
# dataset$State <- factor(dataset$State,
#                         levels = unique(dataset$State),
#                         labels = seq_along(unique(dataset$State)))

# Split into train/test set
# install.packages("caTools")
# caTools library helps split into training/test set
# library(caTools)
# set.seed(123)
# split <- sample.split(dataset$Profit, SplitRatio = 0.8)
# # Training set
# dataset_train <- subset(dataset, split == TRUE)
# # Test set
# dataset_test <- subset(dataset, split == FALSE)

# Feature Scaling
# dataset_train[, 2:3] <- scale(dataset_train[, 2:3])
# dataset_test[, 2:3] <- scale(dataset_test[, 2:3])

# Fitting SVR Model to the dataset
# Create your regressor
# install.packages("e1071")
library(e1071)
svr_reg <- svm(formula = Salary ~ .,
               data = dataset,
               type = 'eps-regression')


# Predicting a new result
y_pred <- predict(svr_reg,
                  newdata = data.frame(Level = 6.5))
y_pred

# Visualizing SVR Results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(svr_reg, newdata = dataset)),
            color = 'blue') +
  ggtitle('Salary vs Position Level (SVR)') +
  xlab('Position Level') +
  ylab('Salary')

# Visualizing Regression Model Results (for higher resolution and smoother curve)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid, y = predict(svr_reg, newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle('Salary vs Position Level (SVR Smooth Grid)') +
  xlab('Position Level') +
  ylab('Salary')

