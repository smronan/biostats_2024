# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
iris <- as_tibble(iris)


# function ----------------------------------------------------------------
## function() is to create a new function

x <- rnorm(100, mean = 10, sd = 100)
hist(x)
sig <- sd(x)
mu <- mean(x)
cv <- sig/mu

##with function
fun_cv <- function(x) {
        cv <- sd(x) / mean(x)
        return(cv)
}

fun_cv(x)

##standardize
x0 <- x - mean(x) #centering
z <- x0 / sd(x0) #scaling

scl <- function(x) {
        x0 <- x - mean(x)
        z <- x0 / sd(x0)
        return(z)
}

scl(x)

##Akira example

scl_2 <- function(s) {
        z <- (s - mean(s)) / sd(s)
        return(z)
}
scl_2(x)


##random function with two arguments

f0 <- function(phi, zeta) {
        cout <- 2 * phi + rnorm(1) * zeta
        return(cout)
}

f0(phi = 2, zeta = 3)

# apply family ------------------------------------------------------------
##apply() for matrix mainly
m <- matrix(rnorm(25), nrow = 5, ncol = 5)
##apply() MARGIN indicates dimension: 1 is row dimension, 2 is column dimension
apply(m, MARGIN = 1, FUN = mean) #calculates mean for each row
apply(m, MARGIN = 2, FUN = mean) #calculate mean for each column
##FUN can be a function you define
apply(m, MARGIN = 1, FUN = fun_cv)

##for dataframe -- difficulty w/dataframe: can contain different data types
apply(iris %>% select(1:4),
      MARGIN = 2, FUN = mean)

##sapply() - for list, vector output
x <- rnorm(10)
y <- rnorm(100)
z <- rnorm(5)

l_xyz <- list(x, y, z)
sapply(l_xyz, FUN = mean)

##lapply() - for list, but output is list too
x <- rpois(10, lambda = 5)
y <- rpois(100, lambda = 5)
z <- rep(letters[1:3], 10)

##create list of xyz
l_xyz <- list(x, y, z)
## remove duplicates, or get unique elements
unique(x)
unique(y)
unique(z)

lapply(l_xyz, FUN = unique)

##try to get only the first elements from each vector
lapply(l_xyz,
       FUN = function(x) {
               x[1]
       })

##2nd example -- better to always specify return()
first <- function(x) {
        return(x[1])
}
lapply(l_xyz, FUN = first)

# for loop ----------------------------------------------------------------

## for loop is to repeat work inside {}
x <- seq(0, 10, by = 0.25)
y <- NULL

## multiply 2 for each element one by one
for (i in 1:10) {
        y[i] <- 2 * x[i] #need to specify where the output is stored y[i]
}
