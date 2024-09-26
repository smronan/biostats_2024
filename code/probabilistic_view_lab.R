# setup -------------------------------------------------------------------

rm(list = ls())
#source("code/set_library.R")
library(tidyverse)
library(patchwork)


# Exercise 1 - Normal Distribution ----------------------------------------
set.seed(1)
x <- rnorm(50, mean = 10, sd = 1)
df_1 <- as.tibble(x)
#df <- as.tibble(x)
mu <- mean(df_1$value)
sigma <- sd(df_1$value)
pd <- dnorm(x, mean = mu, sd = sigma)

df <- tibble(y = pd, x = x)
df %>% 
        ggplot(aes(x = x, y = y)) +
        geom_line() +
        labs(y = "Probability Density")

x_min <- floor(min(x))
x_max <- ceiling(max(x))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:length(bin) - 1) {
        p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) -
                pnorm(bin[i], mean = mu, sd = sigma)
        
}

## probability data frame
df_p <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
        mutate(freq = p * nrow(df_1))
## overlay
df_1 %>% 
        ggplot(aes(x = value)) +
        geom_histogram(binwidth = 1,
                       center = 0.5) +
        geom_point(data = df_p,
                   aes(y = freq,
                       x = bin),
                   colour = "maroon") +
        geom_line(data = df_p,
                  aes(y = freq,
                      x = bin),
                  colour = "maroon")

# Exercise 2 - Poisson Distribution ---------------------------------------

## generate a variable with 1000 observations
set.seed(2)
r <- rpois(1000, lambda = 28)
df_r <- as.tibble(r)
## calculate probability mass
lambda_hat <- mean(df_r$value)
pm <- dpois(r, lambda = lambda_hat)

r_min <- min(r)
r_max <- max(r)

bin <- seq(r_min, r_max, by = 1)
## make probability mass figure

tibble(y = pm, x = r) %>% 
        ggplot(aes(x = x, y = y)) +
        geom_line(linetype = "dashed") +
        geom_point() +
        labs(y = "Probability",
             x = "Count")
df_prob <- tibble(x = r, y = pm) %>% 
        mutate(freq = y * nrow(df_r))

## overlay figures

df_r %>% 
        ggplot(aes(x = value)) +
        geom_histogram(binwidth = 0.5,
                       center = 0) +
        geom_line(data = df_prob,
                  aes(x = x,
                      y = freq),
                  linetype = "dashed") +
        geom_point(data = df_prob,
                   aes(x = x,
                       y = freq))



