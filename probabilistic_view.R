# setup -------------------------------------------------------------------

rm(list = ls())
#source("code/set_library.R")
library(tidyverse)
library(patchwork)

# load csv data on R
df_h0 <- read_csv("data_raw/data_plant_height.csv")

# class work --------------------------------------------------------------

## histogram
df_h0 %>% 
        ggplot(aes(x = height)) +
        geom_histogram() +
        geom_vline(aes(xintercept = mean(height)))

## vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
        ggplot(aes(x = x, y = y)) +
        geom_line() + # draw lines
        labs(y = "Probability density") # re-label
## calculate the area under the curve to translate to probabilities
# probability of x < 10
p10 <- pnorm(10, mean = mu, sd = sigma)
print(p10)
# probability of x < 5
p5 <- pnorm(5, mean = mu, sd = sigma)
# probability of 5 < x < 10
p10-p5

## create histogram with estimates
x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by = 1) #increments of 1 from min to max

## calculate probabilities for each bin
p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
        p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) - 
                pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
        mutate(freq = p * nrow(df_h0))

## Overlay estimated values of df_prob onto histogram
df_h0 %>% 
        ggplot(aes(x = height)) +
        geom_histogram(binwidth = 1,
                       center = 0.5) +
        geom_point(data = df_prob, #switch data frames for probability estimates
                   aes(y = freq,
                       x = bin),
                   colour = "maroon") +
        geom_line(data = df_prob,
                  aes(y = freq,
                      x = bin),
                  colour = "maroon")

# class work: discrete variables ------------------------------------------
df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)

# make a histogram
df_count %>% 
        ggplot(aes(x = count)) +
        geom_histogram(binwidth = 0.5,
                       center = 0)

# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass for each value
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat) # directly returns probability for each x value

# figure
tibble(y = pm, x = x) %>% # data frame
        ggplot(aes(x = x, y = y)) +
        geom_line() + 
        geom_point() + # draw points
        labs(y = "Probability",
             x = "Count") # re-label

## overlay expected frequency on the figure
df_prob <- tibble(y = pm,
                  x = x) %>% 
        mutate(freq = y * nrow(df_count))

df_count %>% 
        ggplot(aes(x = count)) + 
        geom_histogram(binwidth = 0.5,
                       center = 0,
                       fill = "lightblue",
                       colour = "darkblue") +
        geom_line(data = df_prob,
                  aes(x = x,
                      y = freq),
                  linetype = "dashed",
                  colour = "maroon") +
        geom_point(data = df_prob,
                   aes(x = x,
                       y = freq),
                   colour = "maroon")


