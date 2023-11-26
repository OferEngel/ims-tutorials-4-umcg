# November 24 2022

binom.test(x = 180, n = 900)
45 / 180
(45 + 255) /900
dbinom(3, 3, 550 / 900)
(550 / 900)^3
dbinom(7, 10, .85)

library(tidyverse)

df <- tribble(
  ~sex, ~outcome, ~freq,
  "male", "survive", 367 ,
  "male", "dead", 1364,
  "female", "survive", 344,
  "female", "dead", 126
) |> uncount(freq) |>
  table() |>
  chisq.test()


24
