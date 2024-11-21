# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(dplyr)
# Perform only once
# install.packages("palmerpenguins")
library(palmerpenguins)


# data cleaning -----------------------------------------------------------

df_pen <- as_tibble(penguins_raw)
# penguins_raw <- penguins_raw %>% 
#         rename_with(str_to_lower(c(colnames(penguins_raw)))
# penguins_raw <- rename_with(penguins_raw, tolower)

df_pen <- rename_with(df_pen, ~ tolower(gsub(" ","_", .x, fixed = TRUE))) 
cnm <- colnames(df_pen)
cnm_clean <- cnm %>% 
        str_replace_all("_\\(mm\\)", "") %>% 
        str_replace_all("_\\(g\\)", "") %>% 
        str_replace_all("_\\(o/oo\\)", "")
colnames(df_pen) <- cnm_clean        

df_pen <- df_pen %>% 
        mutate(success = ifelse(clutch_completion == "Yes",
                                yes = 1,
                                no = 0),
               species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                                   species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                                   species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap")) %>% 
        drop_na(culmen_length,
                culmen_depth,
                flipper_length,
                body_mass,
                sex)

# analyze penguin data ----------------------------------------------------

m <- glm(success ~ species + 
             culmen_length + 
            culmen_depth + 
            flipper_length + 
            body_mass,
    data = df_pen,
    family = "binomial")

# perform AIC-based model selection 
#install.packages('MuMIn')
library(MuMIn)
options(na.action = "na.fail")
m <- #model object
        m_set <- dredge(m, rank = "AIC")
subset(m_set, delta < 4)
