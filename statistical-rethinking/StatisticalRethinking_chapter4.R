# 1000 people flipping a coin 16 times
flip_results <- replicate(1e3, sum(runif(16, -1, 1)))
plot(flip_results)
plot(density(flip_results))