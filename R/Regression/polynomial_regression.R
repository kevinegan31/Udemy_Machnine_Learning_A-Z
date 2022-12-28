rm(list = ls())
# Polynomial Regression

# Data Preprocessing
# Set working directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 6 - Polynomial Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[2:3]

# Fitting Linear Regression to the dataset
lin_reg <- lm(formula = Salary ~ .,
              data = dataset)

# Fitting Polynomial Regression to the dataset
# Degree = 2
poly_reg_2 <- lm(formula = Salary ~ stats::poly(dataset$Level, 2, raw = TRUE),
                 data = dataset)
# Degree = 3
poly_reg_3 <- lm(formula = Salary ~ stats::poly(dataset$Level, 3, raw = TRUE),
                 data = dataset)
# Degree = 4
poly_reg_4 <- lm(formula = Salary ~ stats::poly(dataset$Level, 4, raw = TRUE),
                 data = dataset)

# Visualizing Linear Regression Results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = 'blue') +
    ggtitle('Salary vs Position Level (Linear Regression)') +
    xlab('Position Level') +
    ylab('Salary')

# Visualizing Polynomial Regression Results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg_4, newdata = dataset)),
            color = 'blue') +
  ggtitle('Salary vs Position Level (Polynomial Regression d = 4)') +
  xlab('Position Level') +
  ylab('Salary')

# Predict single value for Job Level = 6.5
new_df <- data.frame(Level=6.5)
# Linear Regression Prediction
lin_reg_pred <- predict(lin_reg,
                        newdata = new_df)
# Polynomial Regression Prediction
new_poly_df <- data.frame(stats::poly(as.numeric(new_df), 4,
                                      raw = TRUE))
# Using this technique since predict can be challenging with poly
polynomial_predictions <- as.numeric(poly_reg_4$coefficients) * t(cbind(1, new_poly_df))
# Get prediction when degree = 4
polynomial_predictions[5,1]



