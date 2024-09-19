# setup -------------------------------------------------------------------

rm(list = ls())
#source("code/set_library.R")
library(tidyverse)
library(patchwork)
# exercise 1 --------------------------------------------------------------

## obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms 
## of sample means and unbiased variances
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))
mu_100 <- sigma2_ub100 <- NULL

#sample 100 
for (i in 1:100) {
        df_100 <- df_h0 %>% 
                sample_n(size = 100)
        mu_100[i] <- mean(df_100$height)
        sigma2_ub100[i] <- var(df_100$height)
        
}
# sample 50 
mu_50 <- sigma2_ub50 <- NULL
for (i in 1:100) {
        df_50 <- df_h0 %>% 
                sample_n(size = 50)
        mu_50[i] <- mean(df_50$height)
        sigma2_ub50[i] <- var(df_50$height)
        
}

# create data frame with means and variances
sample_50 <- tibble(mu = mu_50,
                var = sigma2_ub50,
                n = 50)
sample_100 <- tibble(mu = mu_100,
                     var = sigma2_ub100,
                     n = 100)
##alternate way to organize
df_m <- bind_rows(sample_50, sample_100)
## alternate way to visualize - combine data frame and figure
df_m %>% 
        ggplot(aes(x = mu,
                   color = factor(n))) +
        geom_histogram()

# histogram for n = 50 -- first attempt using patchwork
g_mu_50 <- sample_50 %>% 
        ggplot(aes(x = mu_hat)) +
        geom_histogram()

g_var_50 <- sample_50 %>% 
        ggplot(aes(x = var_ub)) +
        geom_histogram()

# histogram for n = 100 sample
g_mu_100 <- sample_100 %>% 
        ggplot(aes(x = mu)) + 
        geom_histogram()

g_var_100 <- sample_100 %>% 
        ggplot(aes(x = var)) +
        geom_histogram()
g_mu_100 / g_var_100


# exercise 2 --------------------------------------------------------------

df_h10 <- df_h0 %>% 
        filter(height >= 10)
# mu_h10 <- sigma2_ub_h10 <- NULL
### combination of lapply, function, for loop
# x <- 20
df_m10 <- lapply(X = c(50, 100),
                 function(x){
                
                 mu <- sigma2 <- NULL
                 
                 for(i in 1:100) {
                         df_i <- df_h10 %>% 
                                 sample_n(x)
                         mu[i] <- mean(df_i$height)
                         sigma2[i] <- var(df_i$height)
                 }
                 cout <- tibble(n = x,
                                mu = mu,
                                sigma = sigma2)
                 return(cout)
                 }) %>% 
        bind_rows()
df_m10

# for (i in 1:100) {
#         sample_h10 <- df_h10 %>% 
#                 sample_n(size = 50)
#         mu_h10[i] <- mean(sample_h10$height)
#         sigma2_ub_h10[i] <- var(sample_h10$height)
#         
# }
# df_sample <- tibble(mu_hat = mu_h10,
#                     sigma2 = sigma2_ub_h10)
# g_mu_h10 <- df_sample %>% 
#         ggplot(aes(x = mu_hat)) +
#         geom_histogram()
# g_var_h10 <- df_sample %>% 
#         ggplot(aes(x = sigma2)) +
#         geom_histogram()
# g_mu_h10 / g_var_h10

