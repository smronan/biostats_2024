# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)

df_algae <- read_csv("data_raw/data_algae.csv")
#install.packages("skimr")

skimr::skim(df_algae)

# scatter plot
df_algae %>% 
        ggplot(aes(x = conductivity, y = biomass)) +
        geom_point()
# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

# get estimates
alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
        ggplot(aes(x = conductivity, y = biomass)) +
        geom_point() +
        geom_abline(intercept = alpha,
                    slope = beta)

# get residuals 
eps <- df_algae$biomass - (alpha + (beta*df_algae$conductivity))
eps0 <- resid(m) #performs the equation above
ss <- sum(eps^2)

## get t-values
# extract coefficients
theta <- coef(m)

se <- sqrt(diag(vcov(m)))

t_value <- theta / se

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + #upper tail
        pt(-t_value[1], df = 48) #lower tail

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + #upper tail
        pt(-t_value[2], df = 48) #lower tail
print(p_alpha) 

## visualize errors
# get residuals 
eps <- df_algae$biomass - (alpha + (beta*df_algae$conductivity))
eps0 <- resid(m) #performs the equation above
ss <- sum(eps^2)
# add error column
df_algae <- df_algae %>% 
        mutate(eps = eps)
# visualizing deviation from the line
df_algae %>% 
        ggplot(aes(x = conductivity,
                   y = biomass)) +
        geom_point() +
        geom_abline(intercept = alpha,
                    slope = beta) + 
        geom_segment(aes(x = conductivity, # start-coord x
                         xend = conductivity, # end-coord x
                         y = biomass, # start-coord y
                         yend = biomass - eps), # end-coord y
                     linetype = "dashed")

