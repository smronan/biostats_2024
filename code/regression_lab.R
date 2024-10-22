# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)

df <- as_tibble(iris)
# subset data frame into 3 separate df for each species
df_set <- subset(df, Species == "setosa")
df_vc <- subset(df, Species == "versicolor")
df_vir <- subset(df, Species == "virginica")

# perform regression analysis for each species 
set <- lm(Sepal.Width ~ Petal.Width,
          data = df_set)
vc <- lm(Sepal.Width ~ Petal.Width,
         data = df_vc)
vir <- lm(Sepal.Width ~ Petal.Width,
          data = df_vir)
# confirm coefficient of variation manually 
v_y <- df_set %>% 
        pull(Sepal.Width)
v_x <- df_set %>% 
        pull(Petal.Width)
ss <- sum(resid(set)^2)
ss_0 <- sum((v_y - mean(v_y))^2)
r2 <- 1 - ss / ss_0

# multiple explanatory variables ------------------------------------------

## investigate variations in estimates of regression coefficients with 2 explanatory variables

m_s <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data = df_set)
m_v <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
          data = df_vc)
m_vi <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
           data = df_vir)

# for loop? ---------------------------------------------------------------


