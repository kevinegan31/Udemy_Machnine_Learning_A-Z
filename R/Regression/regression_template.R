# Regression Template

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 5 - Multiple Linear Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("50_Startups.csv")
# dataset <- dataset[, 2:3] # subset

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

# Fitting Regression Model to the dataset
# Create your regressor

# Predicting a new result
y_pred <- predict(regressor,
                  newdata = dataframe(Level = 6.5))
y_pred

# Visualizing Regression Model Results (for higher resolution and smoother curve)
library(ggplot2)
x_grid <- seq(min(datset$Level, max(dataset$Level)), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = datframe(Level = x_grid))),
            color = 'blue') +
  ggtitle('Salary vs Position Level (Regression Model)') +
  xlab('Position Level') +
  ylab('Salary')








