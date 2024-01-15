
library(tidyverse)
library(broom)
library(broom.mixed)
library(marginaleffects)
library(faux)
library(lmerTest)
library(merTools)
library(GGally)
conflicted::conflicts_prefer(lmerTest::lmer)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)


set.seed(888)

# y_ij = a_j[i] + b_j[i] * x_i + epsilon_i
#
#              -----------------------------------------------
# a_j[i]      |  alpha_0 |  sig_u^2        , rho*sig_u*sig_v |
#         ~ N |          |                                   |
# b_j[i]      | beta_0   |  rho*sig_u*sig_v, sig_v^2         |
#              ----------------------------------------------


obs_per_cluster <-  100
n_cluster <-  100
# Fixed intercept
alpha_0 <-  0
# Fixed slope
beta_0 <- 0
# Random intercept st.dev (level 2)
sig_u <-  1.5
# Random slope st.dev (level 2)
sig_v <-  2
# Independent random effects
cor <-  0.5
# Residual standard deviation
sig <- 1


# step 1: create the units
df <- add_random(
  cluster = n_cluster,  # first the clusters
  obs = obs_per_cluster # second, the observations
) |>
  # step 2: Add anything at level 2
  # Here we have random intercept/slope
  add_ranef(
    "cluster",
    u_i = sig_u,
    v_i = sig_v,
    .cors = cor
  ) |> mutate(
    a_i = alpha_0 + u_i,
    b_i = beta_0  + v_i
  ) |>
  # step 3: add anything at level 1
  mutate(
    x       = rnorm(n_cluster * obs_per_cluster),
    epsilon = rnorm(n_cluster * obs_per_cluster, 0, sig),
    y = a_i + b_i * x + epsilon,
    y_hat = a_i + b_i * x
  )


# Print the result
# try to figure out what just happened
df |>
  print(n = 20)

# For the models, we really only need
# the cluster id, the y and the x

df  <- df |> select(cluster, x, y, y_hat)

# Print the result
# try to figure out what just happened
df |>
  print(n = 20)


mdl <- lmer(y ~  x + (x | cluster), data = df )
summary(mdl)

p.ranef <- ranef(mdl) |>
  as_tibble() |>
  filter(term == "(Intercept)") |>
  mutate(
    cluster = grp,
    ref.est = condval,
    ref.se = condsd,
    .keep = "none"
    )
p.me <- predictions(
  mdl, by = "cluster"
  ) |>
  as_tibble() |>
  mutate(
    cluster = cluster,
    me.est = estimate,
    me.se = std.error,
    .keep = "none"
  )

p.pe <- predictInterval(
  mdl,
  newdata = data.frame(
    cluster = unique(df$cluster),
    x = 0
    )
  ) |>
  mutate(
    pe.est = fit,
    pe.se  = abs(lwr - upr) / (2 * 1.96),
    cluster = unique(df$cluster),
    .keep  = "none"
  ) |>
  as_tibble()


(comp <- p.ranef |>
    inner_join(p.me) |>
    inner_join(p.pe))


# Are the estimates proportional?
ggpairs(comp, columns = c(2, 4, 6))

comp |>
  lm(me.est ~ ref.est, data = _) |>
  summary()

comp |>
  lm(me.est ~ pe.est, data = _) |>
  summary()

comp |>
  lm(ref.est ~ pe.est, data = _) |>
  summary()

comp |>
  select(ref.est, me.est, pe.est) |>
  pivot_longer(
    1:3,
    names_to = "method",
    values_to = "estimate"
  ) |>
  ggplot(aes(method, estimate)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c("marginal effects", "simulations", "random effects")
  )



##################################
# Are the se's proportional?
# The marginal effects are much larger than the
comp |>
  lm(me.se ~ ref.se, data = _) |>
  summary()

# No correlation between the marginal effects se and the
# simulation's standard errors
comp |>
  lm(me.se ~ pe.se, data = _) |>
  summary()

# No correlation between the random effects se and the
# simulation's standard errors
comp |>
  lm(ref.se ~ pe.se, data = _) |>
  summary()

comp |>
  select(ref.se, me.se, pe.se) |>
  pivot_longer(
    1:3,
    names_to = "method",
    values_to = "estimate"
  ) |>
  ggplot(aes(method, estimate)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "marginal effects",
      "simulations",
      "random effects"
    )
  )

ggpairs(comp, columns = c(3, 5, 7))



#################################################
# RADON CASE
# https://georgewoolsey.github.io/ess575_MultilevelHierarchicalModels_GelmanHill/the-data.html#the-data

# our goal in analyzing these data was to estimate the distribution of radon levels in each of the approximately 3000 counties in the United States, so that homeowners could make decisions about measuring or remediating the radon in their houses based on the best available knowledge of local conditions. For the purpose of this analysis, the data were structured hierarchically: houses within counties. If we were to analyze multiple measurements within houses, there would be a three-level hierarchy of measurements, houses, and counties. In performing the analysis, we had an important predictor—the floor on which the measurement was taken, either basement or first floor; radon comes from underground and can enter more easily when a house is built into the ground. We also had an important county-level predictor—a measurement of soil uranium that was available at the county level…The hierarchical model allows us to fit a regression model to the individual measurements while accounting for systematic unexplained variation among the 3000 counties. (p. 3)
#

srrs2 <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/radon/srrs2.dat", header=TRUE, sep=",")
# srrs2 <- read.table("../data/radon/srrs2.dat", header=TRUE, sep=",")
srrs2 %>%
  glimpse()
srrs2 |> mutate(county = stringr::str_trim(county)) |>
  mutate(across(where(is.character), as.factor)) |>
  readr::write_rds("srrs2.rds")

srrs2 <- readr::read_rds("srrs2.rds")
# srrs2 <- read.table("../data/radon/srrs2.dat", header=TRUE, sep=",")
srrs2 %>%
  glimpse()
srrs2 |> mutate(county = stringr::str_trim(county)) |>
  mutate(across(where(is.character), as.factor)) |>
  readr::write_rds("srrs2.rds")


# filter MN and create vars
radon_mn <- srrs2 %>%
  filter(
    state=="MN"
  ) %>%
  mutate(
    county = fct_drop(county)
    , radon = activity
    , log.radon = log(ifelse(radon==0, .1, radon))
    , y = log.radon
    , x = floor # 0 for basement, 1 for first floor
    , county_index = as.numeric(as.factor(county))
    , .keep = "used"
  )
# n
n <- nrow(radon_mn)

# count
radon_mn |>
  count(county_index, county) |>
  slice_head(n=10)
y <- radon_mn$y
x <- radon_mn$x
county <- radon_mn$county
county <- radon_mn$county_index


# y_i = alpha_j[i] + epsilon_i
# alpha_j   ~ N(0, sig_alpha^2)
# epsilon_i ~ N(0, sig_y^2)
#
library(lmerTest)
library(merTools)
library(arm)
conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(lmerTest::lmer)

mdl <- radon_mn |>
  lmer(log.radon ~ 1 + (1 | county ), data = _)
summary(mdl)

# mu_alpha = 1.312
# sig_alpha = 0.3095
# sig_y = 0.7979
###########
# From http://www.stat.columbia.edu/~gelman/arm/examples/radon/radon1.R
#

# 1.  No-pooling and complete-pooling models

library(arm)

# fit the models

lm.pooled <- lm(log.radon ~ floor, data = radon_mn)
display (lm.pooled)
# lm(formula = log.radon ~ floor, data = radon_mn)
# coef.est coef.se
# (Intercept)  1.33     0.03
# floor       -0.61     0.07


lm.unpooled.0 <- lm (y ~ x + county)
display (lm.unpooled.0)

lm.unpooled <- lm (y ~ x + factor(county) - 1, data = radon_mn)
display (lm.unpooled)

# graph the data and fitted lines

x.jitter <- x + runif(n,-.05,.05)
display8 <- c (36, 1, 35, 21, 14, 71, 61, 70)  # subset of counties to display
y.range <- range (y[!is.na(match(county, display8))])
radon_mn |>
  filter(county_index %in% display8) |>
  summarise(n = n(), .by = county_index)


plot_j <- function(j) {
  j <- 14
  plot (x.jitter[county==j], y[county==j],
        xlim=c(-.05,1.05),
        ylim=y.range,
        xlab="floor",
        ylab="log radon level",
        cex.lab=1.6,
        cex.axis=1.5,
        pch=20,
        mgp=c(2,.7,0),
        xaxt="n",
        yaxt="n",
        cex.main=1.3
        # , main=unique[j]
        )
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, add=TRUE, col="red")
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, add=TRUE, col="blue")



}

plot_j(36)
# par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
# for (j in display8){
#
# }

# graph no-pooling ests vs. sample size

sample.size <- as.vector (table (county))
J <- length(sample.size)
sample.size.jittered <-
  sample.size * exp(runif(J, -.1, .1))

############

# CDF
# p(Y < y) = p(exp(x) < y) = p(x < log(y))
# x \in [-.1, .1]
# y \in [exp(-.1), exp(.1)]
# y \in c(0.90, 1.10)
#

n_t <- 10
tibble(
  raw = runif(n_t, -.1, .1),
  x = exp(raw),
  y = 1 + raw
) |> select(-raw) |>
  mutate(d = (y-x)/y)

############

# par (mfrow=c(1,1), mar=c(5,5,4,2)+.1)
plot(sample.size.jittered,
      coef(lm.unpooled)[-1],
      cex.lab=1.5, cex.axis=1.5,
      xlab="sample size in county j",
      ylab=expression(
        paste(
          "est. intercept, ",
          alpha[j],
          "   (no pooling)"
          )
        ),
      pch=20,
      log="x",
      ylim=c(.15,3.5),
      yaxt="n",
      xaxt="n"
      )
axis (1, c(1,3,10,30,100), cex.axis=1.5)
axis (2, seq(0,3), cex.axis=1.5)


for (j in 1:J){
  lines(
    rep(sample.size.jittered[j], 2),
    coef(lm.unpooled)[j+1] +
      c(-1,1) * se.coef(lm.unpooled)[j+1], lwd=.5)
}

summary(lm.unpooled)

###
###
###
library(broom)
nd <- radon_mn |>
  summarise(n=n(), .by = "county") |>
  as_tibble()

set.seed(777)
df_nopool <- lm.unpooled |>
  tidy() |>slice_tail(n = 85) |>
  mutate(
    county = gsub("factor\\(county\\)", "", term),
    alpha_j = estimate,
    ub = estimate + std.error,
    lb = estimate - std.error,
    .keep = "none"
  ) |>
  inner_join(nd, by = "county")


library(marginaleffects)
# Hardly any
lm.unpooled |>
  predictions(
    newdata = data.frame(county = unique(radon_mn$county), x=0)
    ) |>
  as_tibble() |>
  select(county, estimate, std.error) |>
  mutate(
    em.ub = estimate + std.error,
    em.lb = estimate - std.error
    ) |>
  inner_join(df_nopool, by = "county") |>
  mutate(
    diff.estimate = (estimate  - alpha_j),
    diff.ub = (em.ub - ub) / ub,
    diff.lb = (em.lb - lb) / lb,
    .keep = "none"
    )
# FACIT: Hardly any difference between marginal effects
# and the estimations of the fixed effects per county


df_nopool |>
  mutate(n = n * exp(runif(85, -.2, .2))) |>
  ggplot(aes(n, alpha_j)) +
  geom_point() +
  geom_errorbar(aes(ymin = lb, ymax = ub)) +
  geom_hline(yintercept = mean(y), linetype = "dashed") +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100))




# 2.  Multilevel model

# simplest mlm
# M0 <- lmer (y ~ 1 + (1 | county))
# display (M0)
# lmer(formula = y ~ 1 + (1 | county))
# coef.est  coef.se
# 1.31     0.05
#
# Error terms:
#   Groups   Name        Std.Dev.
# county   (Intercept) 0.31
# Residual             0.80

# including x as a predictor

M1 <- lmer (log.radon ~ floor + (1 | county), data = radon_mn)
display (M1)
options (digits=2)
a.hat.M1 <- fixef(M1)[1] + ranef(M1)$county
b.hat.M1 <- fixef(M1)[2]
predictions(
  M1,
  newdata = datagrid(county = unique(radon_mn$county), floor = 0)
  )  |>
  as_tibble()

# make the graphs

# par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
# for (j in display8){
#   plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
#         xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
#         pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
#   axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
#   axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
#   curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lwd=.5, col="red", add=TRUE)
#   curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, lwd=.5, col="blue", add=TRUE)
#   curve (a.hat.M1[j,] + b.hat.M1*x, lwd=1, col="black", add=TRUE)
# }

# plot of ests & se's vs. sample size
summary(M1)
# se.coef(M1)

a.se.M1 <- se.coef(M1)$county
ranef(M1) |> as_tibble() |>
  add_column(a.se.M1 = as.numeric(a.se.M1)) |>
  mutate(diff.se = (condsd - a.se.M1) / a.se.M1)

## FACIT - condsd is the se for the random effects!



# par (mfrow=c(1,1), mar=c(5,5,4,2)+.1)
plot(
  sample.size.jittered,
  t(a.hat.M1),
  cex.lab=1.5,
  cex.axis=1.5,
  xlab="sample size in county j",
  ylab=expression(
    paste("est. intercept, ", alpha[j], "   (multilevel model)")
    ),
    pch=20,
  log="x",
  ylim=c(.15,3.5),
  yaxt="n",
  xaxt="n")

axis (1, c(1,3,10,30,100), cex.axis=1.5)
axis (2, seq(0,3), cex.axis=1.5)

for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
  a.hat.M1[j,] + c(-1,1)*a.se.M1[j], lwd=.5, col="black")
}

abline (coef(lm.pooled)[1], 0, lwd=.5)

M1 |>


# 3.  Multilevel model including uranium as a county-level predictor

# fit the model using lmer

u.full <- u[county]
M2 <- lmer (y ~ x + u.full + (1 | county))
display (M2)
coef(M2)

a.hat.M2 <- fixef(M2)[1] + fixef(M2)[3]*u + as.vector(ranef(M2)$county)
b.hat.M2 <- fixef(M2)[2]

# make the graphs

par (mfrow=c(2,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", cex.lab=1.6, cex.axis=1.5,
        pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.3, main=uniq[j])
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1.5)
  axis (2, seq(-1,3,2), mgp=c(2,.7,0), cex.axis=1.5)
  curve (a.hat.M1[j,] + b.hat.M1*x, lty=1, add=TRUE)
  curve (a.hat.M2[j,] + b.hat.M2*x, lty=2, add=TRUE)
}

a.se.M2 <- se.coef(M2)$county

par (mfrow=c(1,1), mar=c(5,5,4,2)+.1)
plot (u, t(a.hat.M2), cex.lab=1.5, cex.axis=2.5,
      xlab="county-level uranium measure", ylab="est. regression intercept", pch=20,
      ylim=c(.5,2), yaxt="n", xaxt="n", mgp=c(3.5,1.2,0))
axis (1, seq(-1,1,.5), cex.axis=1.5, mgp=c(3.5,1.2,0))
axis (2, seq(0,2,.5), cex.axis=1.5, mgp=c(3.5,1.2,0))
curve (fixef(M2)["(Intercept)"] + fixef(M2)["u.full"]*x, lty=2, add=TRUE)
for (j in 1:J){
  lines (rep(u[j],2), a.hat.M2[j,] + c(-1,1)*a.se.M2[j,], lwd=.5)
}

# 4.  Some examples of predictions using lmer

# new house in county 26 with x=1

x.squiggle <- 1
a.hat <- fixef(M2)["(Intercept)"] + fixef(M2)["u.full"]*u + unlist(ranef(M2)$county)
b.hat <- fixef(M2)["x"]
sigma.y.hat <- sigma.hat(M2)$sigma$data

y.squiggle <- rnorm (1, a.hat[26] + b.hat*x.squiggle, sigma.y.hat)

n.sims <- 1000
y.squiggle <- rnorm (n.sims, a.hat[26] + b.hat*x.squiggle, sigma.y.hat)

print (quantile (y.squiggle, c(.25,.5,.75)))
print (exp (quantile (y.squiggle, c(.25,.5,.75))))

# new house in a new county

u.squiggle <- mean (u)
g.0.hat <- fixef(M2)["(Intercept)"]
g.1.hat <- fixef(M2)["u.full"]
sigma.a.hat <- sigma.hat(M2)$sigma$county

a.squiggle <- rnorm (n.sims, g.0.hat + g.1.hat*u.squiggle, sigma.a.hat)
y.squiggle <- rnorm (n.sims, a.squiggle + b.hat*x.squiggle, sigma.y.hat)

print (quantile (y.squiggle, c(.25,.5,.75)))

print (exp (quantile (y.squiggle, c(.25,.5,.75))))

# new house in county 2

y.squiggle <- rnorm (n.sims, a.hat[2] + b.hat*x.squiggle, sigma.y.hat)
print (quantile (y.squiggle, c(.25,.5,.75)))


