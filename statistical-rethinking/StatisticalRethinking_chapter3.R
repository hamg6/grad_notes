# common medical example around false positives
pr_positive_result_vampire <- .95
pr_positive_result_mortal <- .01
pr_vampire <- .001
pr_vampire_positive_result <- (pr_vampire * pr_positive_result_vampire) / ((pr_vampire * pr_positive_result_vampire) + ((1-pr_vampire) * pr_positive_result_mortal))

pr_positive <- pr_positive_result_vampire * pr_vampire + pr_positive_result_mortal * (1-pr_vampire)
pr_vampire_positive <- pr_positive_result_vampire * pr_vampire / pr_positive

# posterior sampling using globe tossing example from last chapter... this just regenerates that
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size=9, prob=p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

# lets sample 10K times from this posterior distribution...
# theoretically this sample has same distribution as posterior itself
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(samples)

library(rethinking)
dens(samples)

# this just describes the posterior, which isn't useful
# but we can use these samples to describe and understand posterior...

# w one parameter this is straightforward e.g.,
# add up posterior probability where p < .5
sum(posterior[p_grid < .5])

# do the same drawing from samples
sum(samples < .5) / length(samples)

# get a bit fancier
sum(samples > .5 & samples < .75) / length(samples)

# intervals of defined mass... the Bayesian version of confidence intervals
# typically called 'credibility intervals'
# author likes 'compatible' as in, these intervals are compatible with the data and model

quantile(samples, .8)
quantile(samples, c(.1, .9))
quantile(samples, c(.05, .95))

# take another example, where most common instances are clustered at one end of probability
p_grid <- seq(from = 0, to = 1, length.out=1000)
prior <- rep(1,1000)
likelihood <- dbinom(3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
plot(p_grid, posterior)

# similar to above "percent interval" gives the central 50%
PI(samples , prob = .5)

# another function, "Highest Posterior Density Interval" (HPDI) includes the peak
# of the distribution
HPDI(samples, prob = .5)

# in most cases the two are very similar
# HPDI is more computationally intensive
# it also is much more subject to SIMULATION VARIANCE
# in a case where these two differ wildly, interval may not be a correct summary
# it is likely safer just to plot / show the entire distribution


# final common thing to do is provide point estimates....
# but that's a strange, and dangerous thing to do in this case at hand

# one common metric is "Maximum a posteriori" parameter.. basically where is probability the highest
# this is called MAP... it's basically the same thing as MODE
# it's easy to calculate
p_grid[which.max(posterior)]

# do a similar thing via MCMC, leveraging samples rather than the posterior directly
chainmode(samples, adj = .01)

# these both give you numbers near 1... the naive mean / median are less helpful
mean(samples)
median(samples)

# now lets introduce the idea of loss
# assume the true value is 0.5, the next step would give us the relative cost
# of guessing any other value
sum(posterior * abs(0.5 - p_grid))

# but now lets do this for all values at once, rather than just for 0.5
absolute_loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))

# just so happens that the point that minimizes loss, coincides with the median of the dataset
p_grid[which.min(absolute_loss)]

# the loss function we chose here, absolute loss, will derive a point estimate of the median
# it's other common equivalent is...
quadratic_loss <- sapply(p_grid, function(d) sum(posterior * (d - p_grid)^2))


## moving on from loss functions and sampling from the posterior, we can
## also make claims from the prior
## when we do this w no actual data, and only assumptions, we call it dummy data

# we toss the earth globe 3 times, probability of seeing water is p = .7
dbinom(0:2, size = 2, p=.7) # 9% chance we don't see water; 42% we see it once; 49% we see it twice

# we can generate data like this of an arbitrary size
table(rbinom(100, size = 2, p = .7))

# we can verify it has the same distribution as the dbinom() on line 105
table(rbinom(1e5, size = 2, p = .7)) / 1e5

# simulations with only two tosses aren't that interesting though... expanding back to 9 tosses
dummy_w <- rbinom(1e5, size = 100, p = .6)
simplehist(dummy_w, xlab = "dummy water count")

# estimate for an individual probability
w <- rbinom(1e4, size = 9, prob = .6)
simplehist(w)

# estimate accounting for the entire posterior distribution
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)


# onto the quiz
# data for easy questions
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

# 3E1 how many samples less than .2? 4e-04
length(samples[samples < .2]) / length(samples)

# 3E2 how many samples greater than .8? 11.16%
length(samples[samples > .8]) / length(samples)

# 3E3 how much in between .2 and .8? 88.8%
length(samples[samples < .8 & samples > .2]) / length(samples)

# 3E4 and 3E5 20% of probability is above / below what values? 51.9 and 75.6
quantile(samples, c(.2, .8))

# 3E6 what gives the middle 67% of data? 49.9% and 77.2%
PI(samples, prob = .67)

# 3E7 what gives the middle 67% of data putting ~16.5% on either side of distro? 50.2% and 77.4%
HPDI(samples, prob = .67)

# 3M1 change assumptions to 9 water / 15 tosses, show the posterior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(9, 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(p_grid, posterior)

# 3M2 what's the 90% HPDI? 38.9% and 77.0%
HPDI(samples, prob = .9)

# 3M3  do a posterior predictive check... probably to observe 8
dummy_w <- rbinom(1e5, size = 15, prob = samples)
simplehist(dummy_w)

length(dummy_w[dummy_w == 8]) / length(dummy_w)

# 3M4 using prior from last example, likelihood of 6 waters in 9 tosses
w <- rbinom(1e4, size = 9, prob = samples)
length(w[w==6]) / length(w)

# 3M4 start over from 3M1 with new prior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(p_grid < .5, 0, 1)
likelihood <- dbinom(9, 15, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(p_grid, posterior)

HPDI(samples, prob = .9)

dummy_w <- rbinom(1e5, size = 15, prob = samples)
simplehist(dummy_w)

length(dummy_w[dummy_w == 8]) / length(dummy_w)

w <- rbinom(1e4, size = 9, prob = samples)
length(w[w==6]) / length(w)



#3M6 something you just lifted from this guy: https://gist.github.com/dfucci/46fa63e1cc4aee86d332869107c5372e
library(rethinking)

set.seed(42)
p_grid <- seq(0, 1, length.out=1000)
prior <- rep(1, 1000)
real_water <- 0.7

posteriors <- function(N) {
  likelihod <- dbinom(round(N*real_water), size=N, prob=p_grid)
  posterior <- likelihod * prior
  posterior <- posterior/sum(posterior)
  return(posterior)
}

for (i in 1:3000) {
  posterior <- posteriors(i)
  samples <- sample(p_grid, prob=posterior, size=1e4, replace=T)
  interval <- PI(samples, prob=0.99)
  names(interval) <- NULL
  diff <-interval[2] - interval[1]
  print(diff)
  if (diff < 0.05) {
    print(i)
    break
  }
}




# 3H1
library(rethinking)
data(homeworkch3)
sum(birth1) + sum(birth2)
birth3 <- c(birth1, birth2)


p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, times = 1000)
likelihood <- dbinom(sum(birth3), length(birth3), prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
plot(p_grid, posterior)

p_grid[which.max(posterior)]

# 3H2
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

HPDI(samples, prob = .5)
HPDI(samples, prob = .89)
HPDI(samples, prob = .97)


#3H3
p.samples <- sample(p_grid, size = 1e4, replace = T, prob = posterior)
simul <- rbinom(10000, 200, p.samples)

#3H4 on....
# i'm giving up, i took a vacation in the middleof this quiz
# lets move onto ch4, and hopefully we aren't hopelessly lost