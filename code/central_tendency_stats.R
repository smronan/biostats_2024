# setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)


# central tendency --------------------------------------------------------

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

# calculate arithmetic means for x and y
## for x
mean(x)

## for y
mean(y)
sum(y) / length(y)

# calculate geometric mean
## prod() multiply all elements in a vector
prod(x)^(1 / length(x))

## or mean in a log scale then transform back to an ordinary scale
log_y <- log(y)
exp(mean(log_y))

prod(y)^(1 / length(y))

# calculate median for x and y
## use median()
median(x)
median(y)

## sort y
index <- (length(y) + 1) / 2 #always for odd numbers
sort(y)[index]


# variation  --------------------------------------------------------------

# calculate variance for x and y
## manual calculation
## use sum(), length(), ^

## variance
sig2_x <- sum((x - mean(x))^2)/length(x)

## standard deviation
sqrt(sig2_x)

## variance y
sig2_y <- sum((y - mean(y))^2)/length(y)
## standard deviation y
sqrt(sig2_y)


## quantile range
## quantile()
x25 <- quantile(x, 0.25)
x75 <- quantile(x, 0.75)
iqr_x <- abs(x25 - x75)
iqr_x

yq <- quantile(y, c(0.25,0.75))
(iqr_y <- abs(yq[1] - yq[2]))


# relative variance -------------------------------------------------------

## coefficient of variation
## cv for x and y 
## sd over mean
cv_x <- sqrt(sig2_x)/mean(x)
(cv_y <- sqrt(sig2_y)/mean(y)) #adding () returns value in the console

## IQR / median
(iqr_x / median(x))
(iqr_y / median(y))

## or
diff(quantile(x, c(0.25, 0.75))) / median(x)
abs(diff(quantile(x, c(0.25, 0.75))) / median(x))

