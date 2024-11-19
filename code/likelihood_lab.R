# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# Binomial Distribution ---------------------------------------------------

y <- c(2,2,0,0,3,1,3,3,4,3)
prob <- seq(0,1, by = 0.01)

##likelihood
lh <- NULL
for (i in 1:length(prob)) {
        lh[i] <- prod(dbinom(x = y,
                             size = 10,
                             prob = prob[i]))
}


df_binom <- tibble(prob = prob,
                   lh = lh) %>% 
        arrange(desc(lh)) %>% 
        print()
max_p <- 0.21 * 10
mu_y <- mean(y)


# Normal Distribution -----------------------------------------------------


