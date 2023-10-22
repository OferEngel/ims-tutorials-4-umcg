# http://post8000.svmiller.com/lab-scripts/bayes-lab.html
library(stevedata)
library(brms)
library(tidyverse)
df  <- stevedata::uniondensity

df %>%
  summarize(cor = cor(concen, size))

lm(union ~ concen + left, df) |>
  summary()

lm(union ~ size + left, df) |>
  summary()


lm(union ~ size + concen + left, df) |>
  summary()

union_form <- bf(union ~ left + size + concen)
get_prior(union_form, data=df)

labor_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                  # team labor force size
                  set_prior("normal(-5,2.5)", class = "b", coef="size"),
                  # doesn't think industrial concentration has anything to do with it
                  set_prior("normal(0,10^6)", class="b", coef="concen"),
                  set_prior("normal(0,10^6)", class="Intercept"))

industry_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                     # doesn't think labor force size has anything to do with it
                     set_prior("normal(0,10^6)", class = "b", coef="size"),
                     # team industrial concentration
                     set_prior("normal(10,5)", class="b", coef="concen"),
                     set_prior("normal(0,10^6)", class="Intercept"))


union_mods <- list()

union_mods$"labor" <- brm(union_form,
                          data = df,
                          seed = 8675309,
                          sample_prior = TRUE,
                          refresh = 0,
                          prior = labor_priors)


union_mods$"industry" <- brm(union_form,
                             data = uniondensity,
                             seed = 8675309,
                             sample_prior = TRUE,
                             refresh = 0,
                             prior = industry_priors)


