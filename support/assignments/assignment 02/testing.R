setwd("C:/Users/Ofer/OneDrive - UMCG/Teaching/Statistics UMCG/CPE - Medical statistics/Projects/ims-tutorials-4-umcg/support/assignments/assignment 02")
library(tidyverse)
library(broom)
#
# lvl.fg <- levels(fg_sim$fg_strata)
#
# fg_sim |>
#   mutate(
#     w_strata = fct_recode(fg_strata,
#       w_str1 = "fg_str 1",
#       w_str2 = "fg_str 2",
#       w_str3 = "fg_str 3",
#       w_str4 = "fg_str 4",
#       w_str5 = "fg_str 5"
#     )
#   ) |> select(-fg_strata) -> fg_sim

# save(fg_sim, file = "data/nhanes_fg_sim.Rdata")
load("data/nhanes_fg_sim.Rdata")
head(fg_sim)

# Recreation 1
ggplot(fg_sim, aes(x=smoker)) +
  geom_bar() +
  labs(title = "Distribution of smokers",
       x = "Smoking status",
       y = "Frequency")

lm(fast_gluc ~ waist + smoker + sex, fg_sim) |> summary()



fg_sim |>
  ggplot(aes(x=waist, y = fast_gluc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Waist circumference as a predictor of fasting glucose",
       x = "Waist circumference (cm)",
       y = "Fasting glucose (mmol/L)")



fg_sim |>
  ggplot(aes(x=w_strata, y = fast_gluc)) +
  geom_boxplot() +
  labs(title = "Fasting glucose distribution for each stratum of waist circumference",
       x = "Waist circumference (strata)",
       y = "Fasting glucose (mmol/L)")

fg_sim |>
  group_by(w_strata) |>
  summarise(mn_fg = mean(fast_gluc), sd_fg = sd(fast_gluc), n=n())


fg_sim |>
  group_by(fg_strata)