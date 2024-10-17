# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# exercise 1 --------------------------------------------------------------

## draw violin plot as in Figure 5.1

df_pg <- as_tibble(PlantGrowth)

df_pg %>% 
        ggplot(aes(x = group,
                   y = weight)) +
        geom_violin(draw_quantiles = 0.5,
                    alpha = 0.5) +
        geom_jitter()


# exercise 2 --------------------------------------------------------------

## conduct an ANOVA to see if there are differences in weight between the groups

w <- aov(weight ~ group,
         data = df_pg)
summary(w)

## create a function to calculate F value 
## F value is the ratio of between group variance to within group variance
mu <- mean(df_pg$weight)
f_stat <- function(df) {
s_b <- df %>% 
        group_by(group) %>% 
        summarize(mu_g = mean(weight),
                  dev_g = (mu_g - mu)^2,
                  n = n(),
                  ss = dev_g * n) %>% 
        pull(ss) %>% 
        sum()
s_w <- df %>% 
        group_by(group) %>% 
        mutate(mu_g = mean(weight)) %>% 
        ungroup() %>% 
        mutate(dev_i = (weight - mu_g)^2) %>% 
        group_by(group) %>% 
        summarize(ss = sum(dev_i)) %>% 
        pull(ss) %>% 
        sum()
var_b <- s_b / (n_distinct(df$group) - 1)
var_w <- s_w / (nrow(df) - n_distinct(df$group))
f_value <- var_b / var_w
return(f_value)
}
f_stat(df_pg) 







