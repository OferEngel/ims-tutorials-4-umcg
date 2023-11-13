# https://lindeloev.net/lets-rename-fixed-to-population-level-and-random-to-varying/

# https://solomonkurz.netlify.app/blog/2021-12-16-use-emmeans-to-include-95-cis-around-your-lme4-based-fitted-lines/

# load
library(tidyverse)
library(lme4)
library(brms)
library(patchwork)
library(emmeans)
library(table1)
library(lmerTest)

# adjust the plotting theme
theme_set(
  theme_linedraw() +
    theme(panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "grey92"),
          strip.text = element_text(color = "black", size = 10))
)

data(ChickWeight)
glimpse(ChickWeight)

ChickWeight %>%
  ggplot(aes(x = Time, y = weight, group = Chick)) +
  geom_line(alpha = 3/4, linewidth = 1/4) +
  ylim(0, NA) +
  facet_wrap(~ Diet, labeller = label_both)

table1(~., ChickWeight)
lm(weight ~ Time  + Diet, ChickWeight) |>
  summary()

table(ChickWeight$Chick)


fit1 <- lmerTest::lmer(
  data = ChickWeight,
  weight ~ 1 + Time + (1 + Time | Chick)
)
summary(fit1)

# conditional quadratic growth model
fit2 <- lmerTest::lmer(
  data = ChickWeight,
  weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet +
    I(Time^2):Diet + (1 + Time + I(Time^2) | Chick)
)

summary(fit2)

library(broom)
aug.fit1 <- predict(fit1) %>%
  data.frame() %>%
  set_names("y_hat") %>%
  bind_cols(ChickWeight)

# define and save the predictor values beforehand
nd <- tibble(Time = 0:21)

fit1.predict.population <-
  predict(fit1,
          # notice the two new lines
          newdata = nd,
          re.form = NA) %>%
  data.frame() %>%
  set_names("y_hat") %>%
  bind_cols(nd)
fit1.predict.population

# what have we done?
glimpse(fit1.predict.population)
29.1780 + 10*8.4531
aug.fit1 |> head()
aug.fit1 %>%
  ggplot(aes(x = Time, y = weight, group = Chick, color = Diet)) +
  geom_line(data = fit1.predict.population, aes(Time, y_hat, group = NA), color = "black", linewidth = 2) +
  geom_line(aes(y = y_hat),
            alpha = 3/4,
            linewidth = 1/4) +
  ylim(0, NA) +
  facet_wrap(~ Diet, labeller = label_both)


predict(fit2) %>%
  data.frame() %>%
  set_names("y_hat") %>%
  bind_cols(ChickWeight)  %>%
  ggplot(aes(x = Time, y = weight, group = Chick, color=Chick)) +
  # geom_line(alpha = 3/4, linewidth = 1/4) +
  geom_line(aes(y = y_hat),
            alpha = 3/4,
            linewidth = 1/4) +
  # geom_line(aes(y = y_hat),
  #           alpha = 3/4,
  #           linewidth = 1/4,
  #           color = "blue") +
  ylim(0, NA) +
  facet_wrap(~ Diet, labeller = label_both) +
  theme(legend.position = "none")



predict(fit1) %>%
  data.frame() %>%
  set_names("y_hat") %>%
  bind_cols(ChickWeight)  %>%
  ggplot(aes(x = Time, y = y_hat, group = Chick, color=Diet)) +
  # geom_line(alpha = 3/4, linewidth = 1/4) +
  geom_line(alpha = 3/4,
            linewidth = 1/4) +
  # geom_line(aes(y = y_hat),
  #           alpha = 3/4,
  #           linewidth = 1/4) +
  # geom_line(aes(y = y_hat),
  #           alpha = 3/4,
  #           linewidth = 1/4,
  #           color = "blue") +
  ylim(0, NA) +
  theme(legend.position = "none")

predict(fit1, newdata = data.frame(Time = seq(from = 0, to = 21, length.out = 30)), re.form = NA) |> head()
