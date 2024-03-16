library(tidyverse)

get_ds <- function(n, x_bar, s) {
  x <- rnorm(n, x_bar, s)
  x <- (x - mean(x))
  x <- x / sd(x) * s
  x <- x + x_bar
}




install.packages("probably", dependencies = TRUE)
library(probably)
