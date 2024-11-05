# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# count data --------------------------------------------------------------

df_count <- read_csv("/Users/sophiaronan/Desktop/biostats_2024/data_raw/data_garden_count.csv")
print(df_count)

m_normal <- lm(count ~ nitrate,
               df_count)

summary(m_normal)

# extract estimates
alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

g_normal <- df_count %>% 
        ggplot(aes(x = nitrate,
                   y = count)) +
        geom_point() +
        geom_abline(intercept = alpha,
                    slope = beta)
## fit a poisson model to count data
m_pois <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")
summary(m_pois)

## parameter estimates and their SE values
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se


## make prediction from poisson distribution

df_pred_pois <- df_count %>% 
        reframe(nitrate = seq(min(nitrate),
                              max(nitrate),
                              length = 100),
                y_pois = exp(theta[1] + theta[2] * nitrate))
g_normal +
        geom_line(data = df_pred_pois,
                  aes(y = y_pois),
                  color = "pink")

# offset term -------------------------------------------------------------
## this accounts for un-equal area of sampling
df_count_ue <- df_count %>% 
        mutate(area = rpois(nrow(.), 10),
               count_ue = count * area)

## plot for area vs. count
df_count_ue %>% 
        ggplot(aes(x = area, y = count_ue)) +
        geom_point()

## glm with offset term
m_pois_ue <- glm(count ~ nitrate + offset(log(area)),
                        data = df_count_ue,
                        family = "poisson")
summary(m_pois_ue)

