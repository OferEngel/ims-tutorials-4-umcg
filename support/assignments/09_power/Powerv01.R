# Power calculations:
# https://wise1.cgu.edu/power/calc_exercises.asp
#
# Power Calculation Exercises
# Nationally, performance on a mathematics test for 4th graders is reported to be normally distributed with a mean of 40.0 and a standard deviation of 9.0.

# 1. You would like to know whether the average performance of children in your school district differs from the national average. You believe that a difference as small as 3.0 points is important to detect. How many randomly selected students do you need to include in your sample to have power of 80% to detect a difference of 3.0 points using a two-tailed test with alpha = .05?

# d = 3 / 9
#
d <- 3 / 9
beta <- 0.20
(n <- ((1.96- qnorm(beta)) /d )^2)
(beta <- pnorm(1.96 - d*sqrt(n)))
# Answer 71 students


# Assuming d = 1/3 (mu = 43),
# reject H0 80% of the time (beta = 20%)
map_lgl(
  seq(10000),
  ~ mean(rnorm(71, 43, 9)) > 40 + 1.96 * 9/sqrt(71)
  ) |> mean()


# If H0 is true, we fail to reject 5% of the time
map_lgl(
  seq(10000),
  ~ abs(mean(rnorm(71, 40, 9)) - 40) >= 1.96 * 9/sqrt(71)
) |> mean()


# 2. Your friend Bumble collected data from a sample of 25 children from a large school where the population mean is 45.0. If he uses a one-tailed test with alpha = .01, how likely is he to attain statistical significance? That is, what is his statistical power?
#

zc <- -qnorm(.01)
d <- 5 / 9
n <- 25
1 - pnorm(zc - d*sqrt(n))

