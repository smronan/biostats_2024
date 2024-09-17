# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") %>% # unit
        mutate(mu_height = mean(height),
               var_height = sum((height - mu_height)^2) / nrow(.)) #"." represents df when piping

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
        mutate(mu_height = mean(height),
               var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

# load csv data into R
## - include 1000 individuals, entire garden
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv")) #read_csv is tidyverse - safer than read.csv

## true mean, true variance
(mu <- mean(df_h0$height))
(sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0))

## random 10 samples
df_i <- df_h0 %>% 
        sample_n(size = 10)
## get 100 sets of 10 samples and estimate mean for each 10
mu_i <- sigma2_i <- NULL #define null objects for indexing to work
for (i in 1:100) {
        df_i <- df_h0 %>% 
                sample_n(size = 10)
        mu_i[i] <- mean(df_i$height)
        sigma2_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
}
## ubiased estimate
mu_i <- sigma2_i <- sigma2_u_i <- NULL #define null objects for indexing to work
for (i in 1:100) {
        df_i <- df_h0 %>% 
                sample_n(size = 10)
        mu_i[i] <- mean(df_i$height)
        sigma2_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i)
        sigma2_u_i[i] <- var(df_i$height)
}

library(patchwork)

## draw histogram for mean
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = sigma2_i,
                    var_u_hat = sigma2_u_i)
g_mu <- df_sample %>% 
        ggplot(aes(x = mu_hat)) +
        geom_histogram() +
        geom_vline(xintercept = mu)

g_var <- df_sample %>% 
        ggplot(aes(x = var_hat)) +
        geom_histogram() +
        geom_vline(xintercept = sigma2)

g_var_u <- df_sample %>% 
        ggplot(aes(x = var_hat)) +
        geom_histogram() +
        geom_vline(xintercept = sigma2) +
        scale_x_continuous(limits= c(min(c(sigma2, sigma2_u_i)),
                                     max(c(sigma2, sigma2_u_i))))

## using patchwork
g_mu / g_var / g_var_u # slash alines vertically


