rm(list = ls())
# Eclat

# Data Preprocessing

# Set working directory
setwd(paste("/Users/kevinegan/Documents/Udemy Courses/",
            "Machine Learning A-Z/Machine Learning A-Z (Codes and Datasets)/",
            "Part 5 - Association Rule Learning/Section 29 - Eclat/R/", sep = ""))
# Import the dataset
# install.packages("arules")
library(arules)
# dataset <- read.csv("Market_Basket_Optimisation.csv",
#                     header = FALSE)
dataset <- read.transactions("Market_Basket_Optimisation.csv",
                             sep = ",",
                             rm.duplicates = TRUE)
summary(dataset)
# Visualize frequency of different products bought throughout the week
itemFrequencyPlot(dataset, topN = 100)

# Training Apriori on the dataset
# Minimum support we want to have in our rules
# Minimum confidence we want to have in our rules
# Support - Consider products purchased 4 times a day, over 7 days a week, divided by the number of items in the dataset
# Default confidence value divided by 2 to get relavent rules
rules <- eclat(data = dataset,
                 parameter = list(support = round(4 * 7 / (nrow(
                   dataset
                 )), 3),
                 minlen = 2))

# Visualizing the results
inspect(sort(rules, by = "support")[1:10])
