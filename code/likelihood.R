# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# poisson example, likelihood -----------------------------------------------

# dpois()
# the first argument is "k" = the value at which you are evaluating the probability
# the second argument is "lambda" = the mean of the dsitribution
# Pr(y = 3) = 3.5^3 exp(-3.5) / 3!
dpois(3, lambda = 3.5)

# write an equation
# if the data follows poisson distribution, this is the probability of observing 
## 3 under this assumption
3.5^3 * exp(-3.5) / factorial(3)

# change lambda values, calculate probabilities
lambda <- seq(0, 10, by = 0.1)
pr <- dpois(x = 3, lambda = lambda)
df_pr <- tibble(y = 3, lambda = lambda, pr = pr)
print(df_pr)

## visualize 
df_pr %>% 
        ggplot(aes(x = lambda, y = pr)) +
        geom_point() +
        theme_bw() +
        labs(y = "Pr(y = 3)",
             x = "lambda")

## arrange by probability 
df_pr %>% 
        arrange(desc(pr))

# data y = {3, 2, 5}
pr <- dpois(x = c(3,2,5), lambda = 3)
# probability of observing 3, 2, 5 simultaneously 
# prod(pr)
pr[1] * pr[2] * pr[3]

# likelihood for y = 3,2,5
# lambda = 0 - 10 by 0.01
y <- c(3,2,5)
lambda <- seq(0, 10, by = 0.01)


# the following code automates
# dpois(y, lambda = lamda[1])
# dpois(y, lambda = lamda[2])
# dpois(y, lambda = lamda[3]).....
# probability of three data points at once
pr <- sapply(X = lambda, 
       FUN = function(z) prod(dpois(y, lambda = z)))
# make a data frame and arrange by pr (likelihood)
df_pois <- tibble(lambda = lambda,
                  pr = pr)

df_pois %>% 
        arrange(desc(pr)) %>% 
        print()

# visualize
df_pois %>% 
        ggplot(aes(x = lambda,
                   y = pr)) +
        geom_line() +
        labs(y = "Likelihood")

## extract log likelihood of the fitted model
# load garden plant data
df_count <- read_csv("data_raw/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

logLik(m_pois)
