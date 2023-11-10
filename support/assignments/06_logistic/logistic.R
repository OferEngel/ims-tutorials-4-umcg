library(tidyverse)
library(stevedata)
library(lmtest)
library(broom)
TV16 %>%
  filter(state == "Pennsylvania" & racef == "White") |>
  drop_na() -> Penn


##  Compare Logistic with LPM - "Linear Probability Model
##  Plot residuals LPM
##
M1 <- lm(votetrump ~ age + female + collegeed + famincr + pid7na + ideo + bornagain,
  data = Penn, na.action=na.exclude)

summary(M1)
augment(M1)

M2 <- glm(votetrump ~ age + female + collegeed + famincr + pid7na + ideo + bornagain,
          data = Penn, na.action=na.exclude,
          family = "binomial")

summary(M2)

## Comparing models - is there a difference in sign? significance?
tidy(M1) %>%
  mutate(model = "LPM") %>%
  bind_rows(., broom::tidy(M2) %>% mutate(model = "Logit")) %>%
  arrange(term) %>% filter(term != "(Intercept)")

# LPM: a coefficient communicates an estimated change in the value of y for a one-unit change in x. However, the logistic regression returns coefficients as something called "logits" (i.e. natural logged odds). How might you get more usable information from this model?
#  I selected just this one covariate because it too is binary so the calculation is simple. Further, the coefficient is not too far removed from what it is in the fully specified model.
# Penn <- Penn |> mutate(collegeed = factor(collegeed))
M3 <- glm(votetrump ~ collegeed, data=Penn,
          family = binomial(link="logit"))
tidy(M3) -> tidyM3
interceptM3 <- tidyM3[1, 2] %>% pull()
coefM3 <- tidyM3[2, 2] %>% pull()
M3.lm <- lm(votetrump ~ collegeed, data=Penn)

M3.lm |> tidy()
M3 |> tidy()
# Use M3 to calculate the probability with vs w/o education
library(marginaleffects)
plogis(predict(M3, newdata = data.frame(collegeed = 1))) -
  plogis(predict(M3, newdata = data.frame(collegeed = 0)))
predict(M3.lm, newdata = data.frame(collegeed = 1)) -
  predict(M3.lm, newdata = data.frame(collegeed = 0))

datagrid(collegeed = 0,
         model = M3)
datagrid(collegeed = 0,
         model = M3.lm)
slopes(
  M3,
  newdata = datagrid(
    collegeed = 1
    )
  )

slopes(M3, newdata = "mean")



nd.1 <- data.frame(collegeed = 1, age = 0, female = 0, famincr = 0, pid7na = 0, ideo = 0, bornagain = 1)
nd.0 <- data.frame(collegeed = 0, age = 0, female = 0, famincr = 0, pid7na = 0, ideo = 0, bornagain = 0)
plogis(predict(M2, newdata = nd.1)) -
  plogis(predict(M2, newdata = nd.0))

predict(M1, newdata = nd.1) - predict(M1, newdata = nd.0)
datagrid(collegeed = 0,
         model = M1)
slopes(
  M1,
  newdata = datagrid(
    collegeed = 0
  )
)

datagrid(collegeed = 0,
         model = M2)
slopes(
  M2,
  newdata = datagrid(
    collegeed = 0
  )
)

# Compare with the coefficient of the ML
M3.lm |> tidy() |>
  filter(term == "collegeed") |>
  select(estimate) |> pull()

# Compare with coefficient of the GML / 4
M3 |> tidy() |>
  filter(term == "collegeed") |>
  select(estimate) |>  mutate(estimate = estimate / 4) |>
  pull()


# Compare with anti-logit of the coefficient GML
M3 |> tidy() |>
  filter(term == "collegeed") |>
  select(estimate) |>  mutate(estimate = plogis(estimate)) |>
  pull()
# That would be wrong!

# The “Divide by 4” Rule
tibble(x = seq(-6, 6)) %>%
  ggplot(.,aes(x)) +
  stat_function(fun = plogis) +
  labs(y = "probability", x = "logit (RHS)")

tidy(M2) %>%
  select(1:3) %>%
  mutate(db4 = estimate/4) %>%
  slice(-1)

tidy(M2, exponentiate = TRUE)
augment(M1)
augment(M2) |>
  select(votetrump, starts_with("."))

