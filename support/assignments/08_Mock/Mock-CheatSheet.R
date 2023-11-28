


library(tidyverse)
library(janitor)

df <- tribble(
  ~status, ~predicted, ~freq,
  "true +", "pred +", 990,
  "true +", "pred -", 10,
  "true -", "pred +", 90,
  "true -", "pred -", 8910
) |> uncount(freq)

df |>
  tabyl(status, predicted) |>
  adorn_totals(where = c("row", "col")) |>
  adorn_title()

# sensitivity: prob(pred+ | true+) = 99%
# specificity: prob(pred- | true-) = 99%
# ppv: prob(true+ | pred-) = 990 / 1080 = 91.66667%
binom.test(990, 1080)


df <- tribble(
  ~status, ~predicted, ~freq,
  "true +", "pred +", 99,
  "true +", "pred -", 1,
  "true -", "pred +", 99,
  "true -", "pred -", 9801
) |> uncount(freq)

df |>
  tabyl(status, predicted) |>
  adorn_totals(where = c("row", "col")) |>
  adorn_title()

# sensitivity: prob(pred+ | true+) = 99%
# specificity: prob(pred- | true-) = 99%
# ppv: prob(true+ | pred+) = 99 / 198 = 50%
binom.test(99, 198)




# $$
#   \text{ppv} = p(\text{status}|\text{pred}) = \\
# \frac{p(\text{pred}|\text{status})\cdot~p(\text{status})}{p(\text{pred})}=\\
# \frac{p(\text{pred}|\text{status})\cdot~p(\text{status})}{p(\text{pred}|\text{status})\cdot~p(\text{status}) + p(\text{pred}|\text{~status})\cdot~p(\text{~status})}
# $$




df <- read_csv("https://raw.githubusercontent.com/proback/BeyondMLR/master/data/musicdata.csv")
