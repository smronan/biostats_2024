# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)

df_fish <- read_csv("/Users/sophiaronan/Desktop/biostats_2024/data_raw/data_vpart.csv")

model <- df_fish %>% 
        ggplot(aes(x = distance, y = n_sp)) +
        geom_point()
unique(df_fish$n_sp)
mean(df_fish$n_sp)
var(df_fish$n_sp)


# Develop GLM with appropriate prob distribution --------------------------

## poisson : upper bound = no, variance < mean

fish_pois <- glm(n_sp ~ distance + cat_area + hull_area,
                 data = df_fish,
                 family = "poisson")
summary(fish_pois)

## prediction plot
df_pred_pois <- df_fish %>% 
        reframe(distance = seq(min(distance),
                              max(distance),
                              length = 100),
                cat_area = mean(cat_area),
                hull_area = mean(hull_area)) %>% 
        mutate(log_y_pred = predict(fish_pois,
                                    newdata = .),
               y_pred = exp(log_y_pred))
                
 ## plot
g_fish <- df_fish %>% 
        ggplot(aes(x = distance, y = n_sp)) +
        geom_point() +
        geom_line(data = df_pred_pois,
                  aes(y = y_pred)) +
        theme_bw()
g_fish

# Effect size -------------------------------------------------------------


scale_pois <- glm(n_sp ~ scale(distance) + scale(cat_area) + scale(hull_area),
                  data = df_fish,
                  family = "poisson")
df_pred_2 <- df_fish %>% 
        reframe(distance = seq(min(distance),
                               max(distance),
                               length = 100),
                cat_area = mean(cat_area),
                hull_area = mean(hull_area)) %>% 
        mutate(log_y_pred = predict(scale_pois,
                                    newdata = .),
               y_pred = exp(log_y_pred))              

g_fish <- df_fish %>% 
        ggplot(aes(x = distance, y = n_sp)) +
        geom_point(color = "skyblue3") +
        geom_line(data = df_pred_2,
                  aes(y = y_pred),
                  color = "royalblue") +
        theme_bw()
g_fish







