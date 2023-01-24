# Thompson Sampling - Reinforcement Learning
rm(list = ls())
# Set working directory
setwd(
  paste(
    "~/Documents/Udemy Courses/Machine Learning A-Z/",
    "Machine Learning A-Z (Codes and Datasets)/",
    "Part 6 - Reinforcement Learning/",
    "Section 33 - Thompson Sampling/R",
    sep = ""
  )
)

# Import the dataset
dataset <- read.csv("Ads_CTR_Optimisation.csv")

# Implementing Random Selection
# N <- nrow(dataset)
# d <- ncol(dataset)
# ads_selected <- integer(0)
# total_reward <- 0
# for (n in 1:N) {
#   ad <- sample(1:d, 1)
#   ads_selected <- append(ads_selected, ad)
#   reward <- dataset[n, ad]
#   total_reward <- total_reward + reward
# }
# # Visualising the results
# hist(
#   ads_selected,
#   col = 'blue',
#   main = 'Histogram of ads selections',
#   xlab = 'Ads',
#   ylab = 'Number of times each ad was selected'
# )

# Implementing Upper Confidence Bound (UCB)
N <- nrow(dataset)
d <- ncol(dataset)
ads_selected <- integer()
numbers_of_rewards1 <- integer(d)
numbers_of_rewards0 <- integer(d)
ads_selected <- integer(0)
total_reward <- 0
for (n in 1:N) {
  # Initialize max_random
  max_random <- 0
  # Initialize ad variable
  ad <- 0
  for (i in 1:d) {
    random_beta <- rbeta(n = 1,
                         shape1 = numbers_of_rewards1[i] + 1,
                         shape2 = numbers_of_rewards0[i] + 1)
    if (random_beta > max_random) {
      max_random <- random_beta
      ad <- i
    }
  }
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n, ad]
  if (reward == 1){
    numbers_of_rewards1[ad] <- numbers_of_rewards1[ad] + 1
  } else {
    numbers_of_rewards0[ad] <- numbers_of_rewards0[ad] + 1
  }
  total_reward <- total_reward + reward
}

# Visualising the results
hist(
  ads_selected,
  col = 'blue',
  main = 'Histogram of ads selections',
  xlab = 'Ads',
  ylab = 'Number of times each ad was selected'
)
axis(side=1, at=seq(1,d))
