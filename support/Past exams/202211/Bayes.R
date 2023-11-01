install.packages("bayesmeta", dependencies = TRUE)
library(bayesmeta)
library(metafor)
?escalc
data(CrinsEtAl2014)
crins.es <- escalc(
  measure = "OR",
  ai = exp.AR.events,
  n1i = exp.total,
  ci = cont.AR.events,
  n2i = cont.total,
  slab = publication,
  data = CrinsEtAl2014)

crins.es[, c("publication", "yi", "vi")]

o1 <- (14 / 61) / ((61 - 14) / 61)
o2 <- (15 / 20) / ((20 - 15 )/ 20)
log(o1/o2)



x <- rnorm(100000, 0.1, 0.12)
u_bound <- qnorm(.975, 0, .12)
l_bound <- qnorm(.025, 0, .12)


mean(x > u_bound) + mean(x < l_bound)

data.frame(x = x, significant = (x > u_bound) | (x < l_bound)) |>
  summarise(`mean effect` = mean(x),
            `sd effect` = sd(x),
            prop = n() / 100000,
            .by = significant) |>
  gt::gt() |>
  gt::fmt_number(
    columns = c(2,3),
    decimals = 2
  ) |>
  gt::fmt_percent(
    columns = prop,
    decimals = 1
  )


pnorm(l_bound, 0.1, .12, lower.tail = TRUE) + pnorm(u_bound, 0.1, .12, lower.tail = FALSE)




# Generate data (see comment below)
library(dplyr)
df <-
  data.frame(x = seq(.1 - .12*3, .1 + .12*3, length.out = 100)) %>% mutate(y = dnorm(x, 0.1, .12))

# (gg)plot and (gg)highlight
library(ggplot2)
library(gghighlight)
ggplot(df, aes(x, y)) +
  geom_area(fill = "sky blue") +
  gghighlight(x > u_bound)

