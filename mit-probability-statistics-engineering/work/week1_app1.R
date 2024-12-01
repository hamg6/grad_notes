# 1.1 series vs parallel system failure
P <- .15
n_components <- 1:30
series_failure <- 1 - ( (1 - P) ^ n_components )
parallel_failure <- P ^ n_components

plot(n_components, series_failure, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1),
     xlab = "# components in system", ylab = "% chance of system failure")
lines(n_components, parallel_failure, col = "red", lwd = 2)
legend("topright", legend = c("series", "parallel"), col = c("blue", "red"),
       lty = 1, lwd = 2)


# 1.2 M-out-of-N systems
# probability of two punctured tires on one car trip
n <- 4 # number of tires, total elements in system
m <- 1 # number of failures resulting in system failure
P <- .01 # probability of any tire popping, for simplicity, assuming the spare could pop at any time

pmf <- (factorial(n) / (factorial(m) * factorial(n - m)) ) * ( P ^ m ) * ( (1 - P) ^ (n - m) )
pmf_function <- function(m, n, P) {
  ( factorial(n) / (factorial(m) * factorial(n - m)) ) * ( P ^ m ) * ( (1 - P) ^ (n - m) )
}
cdf_function <- function(m, n, P) {
  n_failures <- 1:n
  pmf <- pmf_function(n_failures, n, P)
  cdf <- sum(pmf[n_failures >= m])
  return(cdf)
}

probabilities <- seq(from = 0, to = 1, length.out = 100)
no_spare <- sapply(probabilities, function (x) cdf_function(1, 4, x))
one_spare <- sapply(probabilities, function (x) cdf_function(2, 5, x))
two_spares <- sapply(probabilities, function (x) cdf_function(3, 6, x))
ten_spares <- sapply(probabilities, function (x) cdf_function(12, 15, x))

plot(probabilities, no_spare, type = "l", col = "blue", lwd = 2,
     ylim = c(0, max(no_spare)),
     xlab = "probability any tire will deflate", ylab = "% chance of trip failure")
lines(probabilities, one_spare, col = "red", lwd = 2)
lines(probabilities, two_spares, col = "purple", lwd = 2)
lines(probabilities, ten_spares, col = "brown", lwd = 2)
legend("topleft", 
       legend = c("no spare", "one spare", "two spares", "ten spares"), 
       col = c("blue", "red", "purple", "brown"),
       lty = 1, lwd = 2)

# 1.3
probabilities <- seq(from = 10e-4, to = 10e-1, length.out = 100)
p1 <- probabilities
p2 <- probabilities ^ 2
p3 <- 3 * probabilities ^ 2 - 2 * probabilities ^ 3
p4 <- 4 * probabilities ^ 3 - 3 * probabilities ^ 4

plot(probabilities, p1, type = "l", col = "blue", lwd = 2,
     ylim = c(0, 1),
     xlab = "probability one engine will fail", ylab = "% chance of trip failure")
lines(probabilities, p2, col = "red", lwd = 2)
lines(probabilities, p3, col = "purple", lwd = 2)
lines(probabilities, p4, col = "brown", lwd = 2)
legend("topleft", 
       legend = c("one engine", "two engines", "three engines", "four engines"), 
       col = c("blue", "red", "purple", "brown"),
       lty = 1, lwd = 2)

# 1.4
T <- 6 # number of hours
lambda <- 1 / 10e4 # rate of engine failures
probabilities <- seq(from=10e-4, to = 10e-1, length.out = 100)
poisson_prob <- 1 - exp(-lambda * T * probabilities)

p1 <- poisson_prob
p2 <- poisson_prob ^ 2
p3 <- 3 * poisson_prob ^ 2 - 2 * poisson_prob ^ 3
p4 <- 4 * poisson_prob ^ 3 - 3 * poisson_prob ^ 4

# summarize at 10% chance of engine failure when incident occurs
names <- c("one engine", "two engines", "three engines", "four engines")
chances_of_failure_p10 <- c(p1[11], p2[11], p3[11], p4[11])
chances_of_failure_p100 <- c(p1[100], p2[100], p3[100], p4[100])
data.frame(names, chances_of_failure_p10, chances_of_failure_p100)

# 1.5
# three systems
# system one: R1, a heater
# system two: R2 and R3, two pumps, one of which must be working
# system three: R4 through R8, five turbines, three of which must be working
# all three systems operate in series... which is the ultimate system we're looking at

s1_fail_rate <- .05
s2_fail_rates <- c(.1, .08)
s3_fail_rates <- c(.2, .17, .09, .15, .15)

s1_uptime <- 1 - s1_fail_rate
s2_uptime <- 1 - prod(s2_fail_rates)

# they're summarization logic is super confusing, so i'm gonna simulate it
s3_uptime_simulation <- data.frame(
  runif(10000000) >= s3_fail_rates[1],
  runif(10000000) >= s3_fail_rates[2],
  runif(10000000) >= s3_fail_rates[3],
  runif(10000000) >= s3_fail_rates[4],
  runif(10000000) >= s3_fail_rates[5]
)
names(s3_uptime_simulation) <- c("t1", "t2", "t3", "t4", "t5")

n_working <- s3_uptime_simulation$t1 + s3_uptime_simulation$t2 + s3_uptime_simulation$t3 + s3_uptime_simulation$t4 + s3_uptime_simulation$t5
s3_uptime <- sum(n_working >= 3) / length(n_working)
system_uptime <- s1_uptime * s2_uptime * s3_uptime

# recreate the chart in the lesson
library(tidyverse)
agg <- s3_uptime_simulation %>%
  group_by(t1, t2, t3, t4, t5) %>%
  summarize(n = n())
print(agg, n = 32)

# explicit question... impact of removing turbine 1
n_working_for_question <- s3_uptime_simulation$t2 + s3_uptime_simulation$t3 + s3_uptime_simulation$t4 + s3_uptime_simulation$t5
s3_uptime_question <- sum(n_working_for_question >= 3) / length(n_working_for_question)
system_uptime_question <- s1_uptime * s2_uptime * s3_uptime_question
