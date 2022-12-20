rm(list=ls())
# Simple Linear Regression

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - ",
            "Regression/Section 4 - Simple Linear Regression/R", sep = ""))

# Read in CSV
salary_df <- read.csv('Salary_Data.csv')

# Split into train/test set
library(caTools)
set.seed(123)
# 20 observations in the training, 10 for test
split <- sample.split(salary_df$Salary,
                      SplitRatio = 2/3)

# Training set
dataset_train <- subset(salary_df,
                        split == TRUE)
# Test set
dataset_test <- subset(salary_df,
                       split == FALSE)

# Fitting Simple Linear Regression to the Training set
simple_regressor <- lm(formula = Salary ~ YearsExperience,
                       data = dataset_train)
summary(simple_regressor)

# Predicting the Test set results
y_pred <- predict(simple_regressor,
                  newdata = dataset_test)

# Visualizing the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset_train$YearsExperience, y = dataset_train$Salary),
             color = 'red') +
  geom_line(aes(
    x = dataset_train$YearsExperience,
    y = predict(simple_regressor,
                newdata = dataset_train)
  ),
  color = 'blue') +
  ggtitle('Salary vs. Experience (Training set)') +
  xlab('Years Experience') +
  ylab('Salary')
# Visualizing the Test set results
ggplot(dataset_test) +
  geom_point(aes(x = YearsExperience, y = Salary),
             color = 'red') +
  geom_line(aes(
    x = YearsExperience,
    y = y_pred
  ),
  color = 'blue') +
  ggtitle('Salary vs. Experience (Test set)') +
  xlab('Years Experience') +
  ylab('Salary')


# With stat_smooth
# Visualizing the Training set results
ggplot(data = dataset_train) +
  geom_point(aes(x = YearsExperience, y = Salary),
             colour = 'red') +
  # your confidence level is the level parameter
  stat_smooth(method="lm", aes(x = YearsExperience, y = Salary),
              se = TRUE, col = 'blue', level = 0.95,) +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')
# Visualizing the Test set results
# With stat_smooth
ggplot(data = dataset_test) +
  geom_point(aes(x = YearsExperience, y = Salary),
             colour = 'red') +
  # your confidence level is the level parameter
  stat_smooth(method="lm", aes(x = YearsExperience, y = Salary),
              se = TRUE, col = 'blue', level = 0.95,) +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')






