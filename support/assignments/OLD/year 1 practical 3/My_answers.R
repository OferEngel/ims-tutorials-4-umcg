library(tidyverse)
library(janitor)
library(haven)
library(mosaic)
library(infer)


df <- read_sav("diabIB.sav")


# 1 Insulin vs. status
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


# 1 Insulin vs. sex

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



# 4 sex vs colourblindness
tribble(~sex, ~colour, ~number,
        "male", "yes", 17,
        "male", "no",  151,
        "female", "yes", 5,
        "female", "no",  147
        ) |>
  uncount(number) |>
  chisq_test(colour ~ sex)




# Q5 - paired t-test
sum.diff <- df |>
  mutate(sbp.diff = sbp2 - sbp1) |>
  summarise(mn.sbp.diff = mean(sbp.diff),
            sd.sbp.diff = sd(sbp.diff))

sum.diff$mn.sbp.diff
sd.err <- sum.diff$sd.sbp.diff / sqrt(nrow(df))
t.value <- sum.diff$mn.sbp.diff / sd.err


df |>
  mutate(sbp.diff = sbp2 - sbp1) |>
  t_test(sbp.diff ~ NULL)

# Q6
df |>
  mutate(sbp.diff = sbp2 - sbp1) |>
  ggplot(aes(sbp.diff)) + geom_histogram()



# 6b - sign test
df |> filter(sbp2 != sbp1) |>
  mutate(sbp.diff.pos = sbp2 - sbp1 > 0) |>
  summarise(n=n(), .by = sbp.diff.pos)

2 * pbinom(115, size = 274, prob = 0.5)

p_hat <- 115 / 274
sd_err <- sqrt(p_hat * (1 - p_hat) / 274)
z <- (p_hat - 1/2) / sd_err
pnorm(p_hat, 1 / 2, sd_err) * 2

wilcox.test(df$sbp1, df$sbp2, paired=TRUE)




# 7 McNemar's test...

# 4 sex vs colourblindness
tribble(~A, ~B, ~number,
        "0", "0", 15,
        "0", "1",  19,
        "1", "0", 6,
        "1", "1",  10
) |>
  uncount(number) |>
  chisq_test(B ~ A)

pbinom(6, 25, p=.5) * 2


# 8 Weight ~ Height + Sex
df |>
  mutate(sex = if_else(sex ==0, "male", "female")) |>
  ggplot(aes(height, weight, color = sex) ) +
  geom_point()



cor.test(df$weight, df$height,
         method=c("pearson"))

cor.test(df$hba1c, df$diabdu,
         method=c("spearman"))



x <- rbinom(1, 10, 0.3)
mn.x <- 4/10
sd_err <- sqrt(mn.x * (1 - mn.x) / 10)
(mn.x - 1/2) / sd_err
