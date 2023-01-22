# Upper Confidence Bound - Reinforcement Learning

# Set working directory
setwd(
  paste(
    "~/Documents/Udemy Courses/Machine Learning A-Z/",
    "Machine Learning A-Z (Codes and Datasets)/",
    "Part 6 - Reinforcement Learning/",
    "Section 32 - Upper Confidence Bound (UCB)/R",
    sep = ""
  )
)

# Import the dataset
dataset <- read.csv("Ads_CTR_Optimisation.csv")

# Implementing Random Selection
N <- nrow(dataset)
d <- ncol(dataset)
ads_selected <- integer(0)
total_reward <- 0
for (n in 1:N) {
  ad <- sample(1:d, 1)
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n, ad]
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

# Implementing Upper Confidence Bound (UCB)
N <- nrow(dataset)
d <- ncol(dataset)
ads_selected <- integer()
numbers_of_selections <- integer(d)
sums_of_rewards <- integer(d)
ads_selected <- integer(0)
total_reward <- 0
for (n in 1:N) {
  # Initialize max_upper_bound variable
  max_upper_bound <- 0
  # Initialize ad variable
  ad <- 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward <- sums_of_rewards[i] / numbers_of_selections[i]
      delta_i <- sqrt((3 / 2) * (log(n) / numbers_of_selections[i]))
      upper_bound <- average_reward + delta_i
    } else {
      upper_bound <-
        1e400 # Infinity, in each round, we'll go through each version of the ad
      # the upper bound will always be set to infinity and we'll repeat through i for each row.
      # Ad will remain equal to 1 until upper_bound changes when the ad is selected.
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound <- upper_bound
      ad <- i
    }
  }
  ads_selected <- append(ads_selected, ad)
  numbers_of_selections[ad] <- numbers_of_selections[ad] + 1
  reward <- dataset[n, ad]
  sums_of_rewards[ad] <- sums_of_rewards[ad] + reward
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
