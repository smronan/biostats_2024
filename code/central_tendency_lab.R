# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

## comparing central tendency measures
# create a new vetor z with with length 1000 as exp(rnorm(n = 1000, 
 # mean = 0, sd = 0.1)) 

z <- exp(rnorm(n = 1000, mean = 0, sd = 1))

# calculate arithmetic mean
mu_z <- mean(z)

# calculate geometric mean
gm_z <- prod(z)^(1 / length(z))

# calculate median
med_z <- median(z)

# draw a histogram of z using functions tibble(), ggplot(), and geom_histogram()
df_z <- as_tibble(z)
g_hist_z <- df_z %>% 
        ggplot(aes(x = value)) +
        geom_histogram(bins = 50)
# draw vertical lines of arithmetic mean, geometric mean, and median on the 
# histogram with different colors using a function geom_vline()
df_z %>% 
        ggplot(aes(x = value)) +
        geom_histogram(bins = 50) +
        geom_vline(xintercept = mu_z, colour = 'blue') +
        geom_vline(xintercept = gm_z, colour = 'red') +
        geom_vline(xintercept = med_z, colour = 'orange')

# create a new vector z_rev as -z +max(z) + 0.1 and repeat steps 1-4
z_rev <- -z + max(z) +0.1 # reversing values in the different direction

# calculate arithmetic mean, geometric mean, median
mu_rev <- mean(z_rev) # arithmetic mean
gm_rev <- exp(mean(log(z_rev))) #prod(z_rev)^(1 / length(z_rev)) this returns infinity - values too large
med_rev <- median(z_rev) # median 

#draw a histogram
df_z_rev <- as_tibble(z_rev)
g_hist_r <- df_z_rev %>% 
        ggplot(aes(x = value)) + 
        geom_histogram(bins = 50)
g_hist_r

# draw vertical lines
df_mu <- tibble(mu = c(mu_rev, gm_rev, med_rev),
                type = c("Arithmetic", "Geometric", "Median"))
g_hist_r +
        geom_vline(data = df_mu,
                   aes(xintercept = mu,
                       color = type)) +
        theme_bw()


# Comparing variation measures --------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 6 elements in w

# convert unit of w to milligram and create a new vector m
m <- w*1000

# calculate SD and MAD for w and m
## SD
sigma_w <- sum((w - mean(w))^2)/length(w)
sigma_m <- sum((m - mean(m))^2)/length(m)
sd_w <- sqrt(sum((w - mean(w))^2)/length(w)) #square root eliminates the units
sd_m <- sqrt(sum((m - mean(m))^2)/length(m))
## MAD w
ad_w <- abs(w - median(w))
mad_w <- median(ad_w)
## MAD m
ad_m <- abs(m - median(m))
mad_m <- median(ad_m)

# calculate CV and MAD/Median for w and m
## CV
cv_w <- sd_w / mean(w)
cv_m <- sd_m / mean(m)
## MAD/Median
mm_w <- mad_w / median(w)
mm_m <- mad_m / median(m)
