rm(list = ls())
# Decision Tree Regression

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 8 - Decision Tree Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[, 2:3] # subset

# Fitting Regression Model to the dataset
# Create your regressor
# install.packages("rpart")
library(rpart)
regressor <- rpart(formula = Salary ~ .,
                   data = dataset)

# Predicting a new result
y_pred <- predict(regressor,
                  newdata = data.frame(Level = 6.5))
y_pred

# Visualizing Regression Model Results (for higher resolution and smoother curve)
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle('Salary vs Position Level (Decision Tree Regression Model)') +
  xlab('Position Level') +
  ylab('Salary')

# Plotting a straight horizontal line.
# Feature scaling is not the problem since trees are based on conditions of the independent variable.
# Has nothing to do with Euclidean distances, which would force us to perform feature scaling.
# Decision tree is just taking the average of all of the data.
# Therfore, the problem is related to the number of splits.

# Create new regressor using control argument. Add splits
regressor <- rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 1))

# Predicting a new result
y_pred <- predict(regressor,
                  newdata = data.frame(Level = 6.5))
y_pred

# Visualizing Regression Model Results (for higher resolution and smoother curve)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid,
                y = predict(regressor, newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle('Salary vs Position Level (Decision Tree Regression Model)') +
  xlab('Position Level') +
  ylab('Salary')

# Solved the problem of the number of splits, but it's still not great.
# Still getting regression line that is not continuous.
