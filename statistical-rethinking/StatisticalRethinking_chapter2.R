### intro... installing packages

#install.packages("tidyverse")
#install.packages("rstan")
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
#library(devtools)
#devtools::install_github("stan-dev/cmdstanr")
#devtools::install_github("rmcelreath/rethinking")

library(rethinking)
library(tidyverse)

### chapter 2
# probability example
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)

## three steps to bayesian modeling
# 1. develop a narrative of how the data came to be
#    1.a. you should always do this to ground yourself, but also discard the story --> more than one story could account your model
# 2. feed that data into your model
# 3. evaluate performance and revise your input assumptions

dbinom(6, size = 9, prob = 0.5)



# simple grid approximation

# define grid and prior
n_points <- 22
p_grid <- seq(from = 0, to = 1, length.out = n_points)
#prior <- rep(1, n_points)
prior <- ifelse(p_grid < .5, 0, 1)
#prior <- exp(-5 * abs(p_grid - .5))

# compute likelihood at each value of grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize posterior so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

# plot the result
plot(p_grid, posterior, type="b",
    xlab = "probability of water", ylab = "posterior probability")
mtext(paste(as.character(n_points), "points"))

# same thing using the quadratic approximation algo that comes w this book
library(rethinking)
globe.qa <- quap(
    alist(
        W ~ dbinom(W+L, p), # binomial likelihood
        p ~ dunif(0, 1) # uniform prior
    ),
    data = list(W=6, L=3) )

## summarize the approximation
precis(globe.qa)


## overkill, but do the same thing with MCMC

n_sample <- 1000
p <- rep(NA, n_sample)
p[1] <- .5
W <- 6
L <- 3

for (i in 2:n_sample) {
  p_new <- rnorm(1, p[i-1], .1)
  if (p_new < 0) p_new <- abs(p_new)
  if (p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom(W, W+L, p[i-1])
  q1 <- dbinom(W, W+L, p_new)
  p[i] <- ifelse(runif(1) < q1/q0, p_new, p[i-1])
}

# compare MCMC output against brute force correct curve
dens(p, xlim=c(0,1))
curve(dbeta(x, W+1, L+1), lty = 2, add=T)



## chapter 2 quiz

#2M1 -- compute and plot grid approximations in the following scenarios
# 1. W, W, W
# 2. W, W, W, L
# 3. L, W, W, L, W, W, W

grid_approx <- function(
      n_points = 20, 
      n_times_water = 3, 
      n_trials = 3
    ) {
  p_grid <- seq(from = 0, to = 1, length.out = n_points)
  #prior <- rep(1, n_points) ## for 2M2
  prior <- ifelse(p_grid < .5, 0, 1)
  likelihood <- dbinom(n_times_water, n_trials, prob = p_grid)
  unstd.posterior <- likelihood * prior
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type="b",
       xlab = "probability of water", ylab = "posterior probability")
  mtext(paste(as.character(n_points), "points"))
}

grid_approx(n_points = 50)
grid_approx(n_points = 50, n_trials = 4, n_times_water = 3)
grid_approx(n_points = 50,n_trials = 7, n_times_water = 5)

# 2M3
# two globes, earth 70% water; mars is 100% land
# if a globe is tossed one and lands on land...
# what is the probability that you have earth
prior_it_is_earth = .5
p_land_given_earth = .3
p_land_given_mars = 1

# drops from 50% chance to 23% change with one toss
p_earth_given_land = (p_land_given_earth * prior_it_is_earth) / ((p_land_given_earth * prior_it_is_earth) + (p_land_given_mars * (1 - prior_it_is_earth)))
p_earth_given_land

# drops from 23% chance to 8% with two tosses
prior_it_is_earth = p_earth_given_land
p_earth_given_land = (p_land_given_earth * prior_it_is_earth) / ((p_land_given_earth * prior_it_is_earth) + (p_land_given_mars * (1 - prior_it_is_earth)))
p_earth_given_land


## 2M4
#deck of three cards
#  one white on both sides
#  one black on both sides
#  one white one side; black the other
# one card pulled at random, lands on table showing black
# show the odds that the other side are black are two thirds

# 6 options; three white; three black
# of the three black options... 
#   two have black on the other side
#   one has white on the other side

## 2M5
# like before but add one more black / black card
# same result... black:

# 8 options; three white; five black
# white wasn't first result, so ignore those
# we got black... 80% chance other side is black

## 2M6
success_odds <- c(2, 1, 0)
weighting <- c(1, 2, 3)

success_odds / sum(success_odds)
(success_odds * weighting) / sum(success_odds * weighting)

## 2M7
# B/B, B/W, W/W... even weighting
# first card is black
# second card is white
# probability other side of first card is black

# the six scenarios...
W -> W
W -> W
W -> B
B -> W
B -> B
B -> B

# if the second card is white, the top two conditions are removed
# of the four remaining scenarios relevant to the hidden first side...
# black has one chance of being behind the white
# it has three chances to be behind the black
# so .75

# you whiffed on the hard ones....
# actually you didn't! just 2H1! you figured out 2H3!!!
# look at the brains on Sam!!

# we want panda problem pr(twins2 | twins1)
# same as.... pr(twins2 | twins1) = pr(twins2, twins1) / pr(twins)
# (you hopped over to bayes too fast, worked ok earlier, not here)

#pr_twins in general is easy
pr_species_a_prior = .5
pr_twins = pr_species_a_prior * .1 + (1-pr_species_a_prior) * .2

#pr_twins_twins is tougher... odds you have two sets of twins in a row?
pr_twins_twins_species_a = .1 * .1
pr_twins_twins_species_b = .2 * .2

pr_twins_twins = (pr_species_a_prior * pr_twins_twins_species_a) + ((1-pr_species_a_prior) * pr_twins_twins_species_b)
pr_twins_twins / pr_twins

pr_species_a_posterior = (pr_species_a_prior * .1) / (pr_twins)

# 2H3
# new test
p_panda_is_species_a = .5
p_test_a_species_a = .8
p_test_a_species_b = .2
p_species_a_given_test_a = (p_panda_is_species_a * p_test_a_species_a) / ((p_panda_is_species_a * p_test_a_species_a) + ((1-p_panda_is_species_a) * p_test_a_species_b))
p_species_a_given_test_a
p_test_a_species_b = .35
p_species_a_given_test_a = (p_panda_is_species_a * p_test_a_species_a) / ((p_panda_is_species_a * p_test_a_species_a) + ((1-p_panda_is_species_a) * p_test_a_species_b))
p_species_a_given_test_a
p_species_a_given_test_a = (p_species_a_given_single * p_test_a_species_a) / ((p_species_a_given_single * p_test_a_species_a) + ((1-p_species_a_given_single) * p_test_a_species_b))
p_species_a_given_test_a = (p_panda_is_species_a * p_test_a_species_a) / ((p_panda_is_species_a * p_test_a_species_a) + ((1-p_panda_is_species_a) * p_test_a_species_b))
p_species_a_given_test_a_posterior = (p_species_a_given_single * p_test_a_species_a) / ((p_species_a_given_single * p_test_a_species_a) + ((1-p_species_a_given_single) * p_test_a_species_b))
p_species_a_given_test_a_posterior
