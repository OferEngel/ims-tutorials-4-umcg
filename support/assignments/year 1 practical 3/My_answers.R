library(tidyverse)
library(janitor)
library(haven)
library(mosaic)

df <- read_sav("diabIB.sav")
df |>
  mutate(
    dead = fct_recode(
      as.character(dead),
      alive = "0",
      CVD   = "1",
      other = "2"
      )
    ) |> drop_na(insulin, dead) |>
  tabyl(insulin, dead) |>
  adorn_totals(where = "row") |>
  adorn_percentages() |>
  adorn_pct_formatting()

# Calculate the risk difference
# the total probability of dying in the insulin group is 0.324 (23 / 71) and in the group of insulin non-users: 0.259 (53 / 205). So the RD = 0.324 – 0.259 = 0.065

# Calculate the risk ratio
# RR = 0.324 / 0.259 = 1.25

# Calculate the odds ratio
# OR = (23 x 152) / (53 x 48) = 1.37
0.324 * (1 - 0.259)  / ((1 - 0.324) * 0.259)



df |>
  mutate(
    sex = fct_recode(
      as.character(sex),
      male = "0",
      female = "1"
    ),
    insulin = fct_recode(
      as.character(insulin),
      no = "0",
      yes = "1"
    )
  ) |> drop_na(insulin, sex) |>
  tabyl(insulin, sex) |>
  adorn_percentages() |>
  adorn_pct_formatting()

df |>
  mutate(
    sex = fct_recode(
      as.character(sex),
      male = "0",
      female = "1"
    )
  ) |> drop_na(insulin, sex) |>
  tabyl(sex, insulin) |>
  chisq.test()


