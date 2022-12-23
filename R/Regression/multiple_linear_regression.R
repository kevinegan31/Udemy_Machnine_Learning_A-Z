# Multiple Linear Regression

# Set Working Directory
setwd(paste("~/Documents/Udemy Courses/Machine Learning A-Z/",
            "Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/",
            "Section 5 - Multiple Linear Regression/R", sep = ""))

# Import the dataset
dataset <- read.csv("50_Startups.csv")
# dataset <- dataset[, 2:3] # subset

# Encoding categorical data
dataset$State <- factor(dataset$State,
                        levels = unique(dataset$State),
                        labels = seq_along(unique(dataset$State)))

# Split into train/test set
# install.packages("caTools")
# caTools library helps split into training/test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
# Training set
dataset_train <- subset(dataset, split == TRUE)
# Test set
dataset_test <- subset(dataset, split == FALSE)

# Fitting Multiple Linear Regression to the Training set
# R removes one of the dummy variables
regressor <- lm(formula = Profit ~ .,
                data = dataset_train)

# Look at the results of our regressor
summary(regressor)

# Predicting the Test set results
y_pred <- predict(regressor,
                  newdata = dataset_test)
y_pred

# Recompute regressor with only R.D. Spend
regressor_rd <- lm(formula = Profit ~ R.D.Spend,
                   data = dataset_train)
# Predicting the Test set results
y_pred_rd <- predict(regressor_rd,
                     newdata = dataset_test)
y_pred_rd

# Building the optimal model using Backward Elimination manually
regressor <-
  lm(
    formula = Profit ~ R.D.Spend +
    Administration +
    Marketing.Spend +
    State,
    data = dataset
  )
summary(regressor)

regressor2 <-
  lm(
    formula = Profit ~ R.D.Spend +
      Administration +
      Marketing.Spend,
    data = dataset
  )
summary(regressor2)

regressor3 <-
  lm(
    formula = Profit ~ R.D.Spend +
      Marketing.Spend,
    data = dataset
  )
summary(regressor3)

regressor4 <-
  lm(
    formula = Profit ~ R.D.Spend,
    data = dataset
  )
summary(regressor4)

# Building the optimal model using Backward Elimination automatically
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(dataset_train, SL)

