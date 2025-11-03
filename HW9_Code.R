################
###HOMEWORK 9###
################

#Author: Aubrey Wendorff
#packs: tidyverse, 


# Objective 1 -------------------------------------------------------------

###PART A###---
library(tidyverse)
set.seed(123) #reproducibility of random values

#Set Parameters
int <- 7
slope <- 2
n <- 100 #number of observations
sigmas <- c(1, 10, 25)
mean <- 0

#Make an empty list
regression_list <- list() 

#Simulate Data for each of the sigmas
for(i in sigmas) {
  xi <- runif(n, 0, 10) #generate 100 random numbers in a unifrom distribution 0-10
  epsilon <- rnorm(n, 0, sd = i) #100 random #'s from a normal dist.
  yi <- int + slope * xi + epsilon #regression equation
  regression_list[[as.character(i)]] <- data.frame(x = xi, y = yi, sigma = i) #make dataframe
}

#Combine df's for each epsilon
sim_data1 <- bind_rows(regression_list)


###PART B###---

#Plot the data in separate graphs for each epsilon
ggplot(sim_data1, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~ sigma, nrow = 1) +
  labs(x = "x", y = "y") +
  theme_bw()

ggsave("Outputs/Obj1.pdf", width = 6, length = 6, units = "in")

# Objective 2 -------------------------------------------------------------

###PART A###---

#Set Parameters
n_flips <- 1:20
p_values <- c(0.55, 0.60, 0.65)
n_sims <- 100

#Make an empty list for results
results <- list()

#Make an nested for loop for the probabilities and flips
for(p in p_values) { #loops over p's
  for(n in n_flips) { #oops over n's
    sig_count <- 0 #counts how many sims result in a sig detection
  
    for(i in 1:n_sims) {
      flips <- rbinom(n, size = 1, prob = p) #sim coin flips
      heads <- sum(flips) #count total heads
      test <- binom.test(heads, n, p = 0.5) #test binomial fairness test
      if(test$p.value < 0.05) { #if -value is < 0.05, then test is unfair
        sig_count <- sig_count + 1 #then we increase our count by 1 when this happens
      }
    }
    results[[length(results) + 1]] <- data.frame(p = p, 
                                                 n_flips = n, 
                                                 prob_det = sig_count / n_sims) #fraction of sig tests
  }
}

#Combine data results in df
sim_data2 <- bind_rows(results)


###PART B###---
ggplot(sim_data2, aes(x = n_flips, y = prob_det, color = factor(p))) + #factor() makes p a categorical variable +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red", "green")) +
  labs(x = "Number of Coin Flips",
       y = "Probability of Unfairness",
       color = "P-value") +
  theme_bw()

ggsave("Outputs/Obj2.pdf", width = 6, length = 6, units = "in")
  