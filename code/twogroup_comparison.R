# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)

# two-group comparison ----------------------------------------------------

df_fl <- read_csv("/Users/sophiaronan/Desktop/biostats_2024/data_raw/data_fish_length.csv")

# group mean and sd
df_fl_mu <- df_fl %>% 
        group_by(lake) %>% # group operation
        summarize(mu_l = mean(length), # summarize by mean()
                  sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
        ggplot(aes(x = lake,
                   y = length)) +
        geom_jitter(width = 0.1, # scatter width
                    height = 0, # scatter height (no scatter with zero)
                    alpha = 0.25) + # transparency of data points
        geom_segment(data = df_fl_mu, # switch data frame
                     aes(x = lake,
                         xend = lake,
                         y = mu_l - sd_l,
                         yend = mu_l + sd_l)) +
        geom_point(data = df_fl_mu, # switch data frame
                   aes(x = lake,
                       y = mu_l),
                   size = 3) +
        labs(x = "Lake", # x label
             y = "Fish body length") # y label

## t-test
x <- df_fl %>%
        filter(lake == "a") %>%  # subset lake a
        pull(length)

y <- df_fl %>%
        filter(lake == "b") %>% # subset lake b
        pull(length)

t.test(x, y, var.equal = TRUE)

## test-statistic -- demonstrating manual calculation
# t-test used for normal data
# pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
        pull(mu_l)

# lake a
print(v_mu[1])

# difference

v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% #data frame to calculate t-statistic
        group_by(lake) %>% # group operation
        summarize(mu_l = mean(length), # summarize by mean()
                  var_l = var(length), # summarize with sd()
                  n = n()) # count number of rows per group

print(df_t)

# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
        ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))
#when variance increases, t-value decreases, reliability decreases
# t-value is a difference between two groups after accounting for how reliable 
# they are


# null hypothesis -------------------------------------------------------

# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)

# draw figure
tibble(x, y) %>% 
        ggplot(aes(x = x,
                   y = y)) +
        geom_line() +
        labs(y = "Probability density",
             x = "t-statistic")
# draw entire range
tibble(x, y) %>% 
        ggplot(aes(x = x,
                   y = y)) +
        geom_line() +
        geom_vline(xintercept = t_value,
                   color = "blue") + # t_value is the observed t_value
        geom_vline(xintercept = abs(t_value),
                   color = "blue") + # t_value is the observed t_value
        labs(y = "Probability density",
             x = "t-statistic") 
# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2)

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)
# p_value
p_value <- pr_below + pr_above
print(p_value)


