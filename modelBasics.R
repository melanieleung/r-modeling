## MODEL BASICS 
# Walk-through of the lesson on modeling data in R from https://r4ds.had.co.nz/model-basics.html 

library(tidyverse)
library(modelr)

options(na.action = na.warn)

# plot your data variables and see relationship
ggplot(sim1, aes(x, y)) + 
  geom_point()

# relationship seems linear, so let's generate some general linear models
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

# quantify distance between model and your data
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

# create measure_distance function
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

# compute overall distance between predicted and actual values (calculate RMSE)
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

# overlay 10 best models over the data; models with least distance are brighter in color
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

# grid search method of systematically trying random models
# parameters picked based on qualitative assessment of best models from previous plot
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

# plots just the 10 best models
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

## NUMERICAL MINIMISATION tool - Newton Raphson search for "best" model
# performed with optim() function
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
#> [1] 4.222248 2.051204

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

## using lm() to fit linear models; another way of finding "best" model if working with linear relationship
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

## PREDICTIONS 
# start with your base "best" model
grid <- sim1 %>% 
  data_grid(x) 
grid

# add predictions
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

# plot predictions
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

## RESIDUALS
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

# one of many ways to visualize residuals and their relationship with the model
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# another plot with the residuals
# it looks randomly scattered, which suggests the model does a good job capturing patterns
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 
