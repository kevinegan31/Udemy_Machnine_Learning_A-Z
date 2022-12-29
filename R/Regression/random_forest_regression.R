rm(list = ls())
# Random Forest Regression

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 9 - Random Forest Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[, 2:3] # subset

# Fitting Regression Model to the dataset
# Create your regressor
# install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor <- randomForest(formula = Salary ~ .,
                          data = dataset,
                          ntree = 100)

# Predicting a new result
y_pred <- predict(regressor,
                  newdata = data.frame(Level = 6.5))
y_pred
# With more trees, we have a better location of the steps in the plot. 
# The trees provide better predictions that are more accurate and representative of the data.


# Visualizing Regression Model Results (for higher resolution and smoother curve)
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid,
                y = predict(regressor,
                            newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle('Salary vs Position Level (Decision Tree Regression Model)') +
  xlab('Position Level') +
  ylab('Salary')

