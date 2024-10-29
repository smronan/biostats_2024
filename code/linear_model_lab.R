# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# Normality assumption ----------------------------------------------------

iris <- as_tibble(iris)
print(iris)
distinct(iris, Species)

m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)
eps <- resid(m_iris)
shapiro.test(eps) ## use shapiro wilk test on residuals


# Model Interpretation ----------------------------------------------------
## Extract intercept values for each species from object m_iris
model_sum <- summary(m_iris)

#First attempt is incorrect because intercepts are related to initial intercept
# int_vers <- model_sum$coefficients[3,1]
# int_virg <- model_sum$coefficients[4,1]
# int_set <- model_sum$coefficients[1,1]

## Akira's example
b <- coef(m_iris) #stores the coefficients
a <- NULL

#intercept for setosa
a[1] <- b[1]

#intercept for versicolor
a[2] <- b[1] + b[3]

#intercept for virginica
a[3] <- b[1] + b[4]


# Alternative model -------------------------------------------------------

# develop a model excluding the Species variable and create a new figure

lm_iris <- lm(Petal.Length ~ Petal.Width,
   data = iris)

df_pred <- iris %>% 
        reframe(Petal.Width = seq(min(Petal.Width),
                                  max(Petal.Width),
                                  length = 100))

y_pred <- predict(lm_iris,
                  newdata = df_pred)
df_pred <- df_pred %>% 
        mutate(y_pred = y_pred)
iris %>% 
        ggplot(aes(x = Petal.Width,
                   y = Petal.Length)) +
        geom_point(alpha = 0.5) +
        geom_line(data = df_pred,
                  aes(y = y_pred))
