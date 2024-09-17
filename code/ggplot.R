
# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
iris <- as_tibble(iris)
# ggplot ------------------------------------------------------------------
##ggplot just prepares the plane for plotting, it does not actually plot

# scatterplot -------------------------------------------------------------

iris %>% 
        ggplot(mapping = aes(x = Sepal.Length,
                             y = Sepal.Width)) +
        geom_point()
##change color by species -- must be within aes()
iris %>% 
        ggplot(mapping = aes(x = Sepal.Length,
                             y = Sepal.Width,
                             color = Species)) +
        geom_point()
##change color uniformly -- must be outside aes()
iris %>% 
        ggplot(mapping = aes(x = Sepal.Length,
                             y = Sepal.Width)) +
        geom_point(color = 'green')

# line --------------------------------------------------------------------

df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)
df0 %>% 
        ggplot(aes(x = x,
                   y = y)) +
        geom_line() +
        geom_point()

# histogram ---------------------------------------------------------------

iris %>% 
        ggplot(aes(x = Sepal.Length)) +
        geom_histogram()

##change bin width
iris %>% 
        ggplot(aes(x = Sepal.Length)) +
        geom_histogram(binwidth = 0.5)

##change how many bins
iris %>% 
        ggplot(aes(x = Sepal.Length)) +
        geom_histogram(bins = 50)
##change the edge color
iris %>% 
        ggplot(aes(x = Sepal.Length)) +
        geom_histogram(color = 'pink')

##change the fill color of the bars
iris %>% 
        ggplot(aes(x = Sepal.Length)) +
        geom_histogram(fill = 'pink',
                       color = 'blue')

# boxplot -----------------------------------------------------------------

iris %>% 
        ggplot(aes(x = Species,
                   y = Sepal.Length)) +
        geom_boxplot()
##change the color of boxes by species
iris %>% 
        ggplot(aes(x = Species,
                   y = Sepal.Length,
                   color = Species)) +
        geom_boxplot()

##change the 'fill' of boxes by species
iris %>% 
        ggplot(aes(x = Species,
                   y = Sepal.Length,
                   fill = Species)) +
        geom_boxplot()

##change the 'fill' and transparency of boxes by species
iris %>% 
        ggplot(aes(x = Species,
                   y = Sepal.Length,
                   fill = Species)) +
        geom_boxplot(alpha = 0.3)


# Practice exercise - 2d density plot -------------------------------------

# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)


# Basic scatterplot
ggplot(data, aes(x=x, y=y) ) +
        geom_point()
##2D histogram
data %>% 
        ggplot(aes(x=x,
                   y=y)) +
        geom_bin2d() +
        theme_bw()
##with IRIS? Not great haha
#iris %>% 
#ggplot(aes(x = Sepal.Length,
#           y = Sepal.Width)) +
#geom_bin2d() +
#theme_bw()

##2D histogram showing contour only
data %>% 
        ggplot(aes(x=x,
                   y=y)) +
        geom_bin_2d()
# Show the area only
ggplot(data, aes(x=x, y=y) ) +
        stat_density_2d(aes(fill = ..level..), geom = "polygon")

# Using raster
ggplot(data, aes(x=x, y=y) ) +
        stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme(
                legend.position='none'
        )  

# practice exercise -- violin plot ----------------------------------------

iris %>% 
        ggplot(aes(x = Species,
                   y = Sepal.Length,
                   fill = Species)) +
        geom_violin()
## data visualization example -- good strategy to understand other people's code
#is to go through it line by line 

