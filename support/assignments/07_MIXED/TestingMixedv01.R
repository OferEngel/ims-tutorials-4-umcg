library(tidyverse)
library(lmerTest)
library(broom.mixed)



library(tidyverse)
library(broom)
library(faux)
n.dams <- 24
n.pups <- 10
scenario.2a <- add_random(dam = n.dams, pup = n.pups) |>
  add_between("dam", dose = c(0, 1, 2, 3)) |>
  mutate(
    p.defect = plogis(-2 + 4/3 * dose),
    defect   = rbinom(n.dams * n.pups, 1, p.defect)
  )
table(scenario.2a$p.defect)


mutate(
  p.def = 0.5,
  defect = rbinom(n.dams * n.pups, 1, p.def),
  scenario = "1.a"
)

##


library(tidyverse)
library(tidyverse)
library(lmerTest)
library(modelsummary)
library(marginaleffects)
library(broom)
library(broom.mixed)
library(faux)

obs_per_cluster <-  10
n_cluster <-  100
beta_0 <-  1
beta_1 <- .5
sigma  <-  1
sd_int <-  .5
sd_slope <-  .25
cor <-  0
seed <-  888

# y = (beta_0 + u0c) + (beta_1 + u1c)*x + epsilon

# epsilon ~ N(0, sigma)
# (u0c, u1c) ~ N(c(0,0), Sigma(sd_int, cor, cor, sd_slope))
set.seed(seed)
df <-
  add_random(
    cluster = n_cluster,
    obs = obs_per_cluster
    ) |>
  add_ranef(
    "cluster",
    u0c = sd_int,
    u1c = sd_slope,
    .cors = cor
    ) |>
  mutate(
    x       = rnorm(n_cluster * obs_per_cluster),
    epsilon = rnorm(n_cluster * obs_per_cluster),
    y = (beta_0 + u0c) + (beta_1 + u1c) * x + epsilon
    )


# Complete pooling
M_complete <- df |>
  lm(y ~ x, data = _)

# No pooling
M_none <- df |>
  lm(y ~ x + cluster + cluster:x, data = _)
M_none |>
  tidy() |>
  mutate(term = str_replace(term, "clustercluster", "cluster"))

# Partial pooling
M_partial <- df |>
  lmer(y ~ x + (1 + x|cluster), data = _)
M_partial |>
  tidy()



# RADON CASE
# https://georgewoolsey.github.io/ess575_MultilevelHierarchicalModels_GelmanHill/the-data.html#the-data



# srrs2 <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/radon/srrs2.dat", header=TRUE, sep=",")

# srrs2 <-
#   read.table("srrs2.data", header=TRUE, sep=",") |>
#   mutate(
#     radon = activity
#     , log.radon = log(ifelse(radon==0, .1, radon))
#     , first_floor = floor
#     , county = str_trim(county)
#     , state  = str_trim(state)
#     , county_index = as.numeric(as.factor(county))
#     # , .keep = "none"
#   )
#
# srrs2 %>%
#   dplyr::glimpse()
# radon_mn <- srrs2 %>%
#   filter(
#     state=="MN"
#   )
#
# - floor
# - county
# - state
# - log.radon
# -  u county uranium measurement

radon <- read_rds("radon.rds")
radon_mn <- radon |> filter(state == "MN") |>
  mutate(county = fct_drop(county))

radon_mn |>
  summarize(mn=mean(log.radon), md = median(log.radon),
            m = min(log.radon), M = max(log.radon), sd = sd(log.radon), n=n(), .by = county) |> arrange(m) |>
  print(n = 100)


radon_mn_count <- radon_mn |>
  count(county, name = "n.obs")

# m.comp_pool <- radon_mn |>
#   lm(log.radon ~ 1, data = _)
# m.no_pool <- radon_mn |>
#   lm(log.radon ~ 0 + county, data = _)
# m.partial_pool <- radon_mn |>
#   lmer(log.radon ~ (1|county), data = _)
#
#
# m.comp_pool.prep <- predictions(m.comp_pool)[1, ] |>
#      as.data.frame()

library(modelsummary)
library(marginaleffects)
library(broom)
library(broom.mixed)
msummary(list(m.comp_pool, m.no_pool, m.partial_pool),
         stars = TRUE)

get_radon_vs_n_obs <- function (mdl) {
  set.seed(888)
  x.seq <- c(1 , seq(2, 10, 2), 10, seq(20, 100,20))
  p <- mdl  |>
    predictions(newdata = datagrid(county = unique)) |>
    inner_join(radon_mn_count, by = "county") |>
    mutate(
      n.obs.jitter = rnorm(
        nrow(radon_mn_count)
        , n.obs
        , n.obs / 10
        )
      ) |>
    ggplot(
      aes(
        x = n.obs.jitter,
        xend = n.obs.jitter,
        y = conf.low,
        yend = conf.high
        )
      ) +
    geom_point(aes(y = estimate)) +
    geom_segment() +
    scale_x_log10(breaks = x.seq) +
    geom_hline(
      yintercept = c(m.comp_pool.prep$estimate)
      ) +
    labs(x = "Number of observations", y = "Estimated radon in the county", subtitle = "Estimated log Radon vs. number of observations")
  return(p)
}

get_radon_vs_n_obs(m.no_pool) +
  labs( title = "Radon measurements in in Minnesota (no pooling)")
get_radon_vs_n_obs(m.partial_pool) +
  labs( title = "Radon measurements in in Minnesota (multilevel, partial pooling)")

# Average levels vs. number of observations vs.


# Varying intercepts, no predictors
radon_mn |>
ggplot(aes(log.radon)) +
  geom_histogram()

M_no_pooling <- lm(log.radon)


M0 <- lmer(log.radon ~ 1 + (1|county_index), data = radon_mn)
summary(M0)


## Figure 12.4
##


my.counties <- radon_mn |>
  count(county) |>
  filter(n>0) |>
  filter(
    str_detect(
      county,
      "PARLE|BLUE|KOOCH|DOUG|CLAY|STEAR|RAMS|LOUI|MCLEOD|DAKO|OTTER|MOWER|RICE|SCOTT|MILLE LACS|REDWOOD"
      )
    ) |>
  pull(county) |>
  as.character() |>
  as.factor()

# radon_mn |> count(county, first_floor) |>
#   pivot_wider(id_cols = county, names_from = first_floor, values_from = n) |>
#   print(n=1000)

# m.ff.c_pool <- radon_mn |>
#   lm(log.radon ~ first_floor, data = _)
# summary(m.ff.c_pool)
# m.ff.no_pool <- radon_mn |>
#   lm(log.radon ~ first_floor + county, data = _)
# m.ff.prtl_pool <- radon_mn |>
#   lmer(log.radon ~ first_floor + (1|county), data = _)


m.ff.c_pool <- radon_mn |>
  lm(log.radon ~ first_floor, data = _)
# summary(m.ff.c_pool)
m.ff.no_pool <- radon_mn |>
  lm(log.radon ~ (first_floor + county)^2, data = _)
# summary(m.ff.no_pool)
m.ff.prtl_pool <- radon_mn |>
  lmer(log.radon ~ first_floor + (first_floor|county), data = _)
# ranef(m.ff.prtl_pool) |> as_tibble() |> mutate(t = condval/condsd) |>
# arrange(t) |> print(n = 1000)



# m.ff.c_pool |>
#   predictions(
#     newdata = datagrid(
#       first_floor = c(0, 1))
#     ) |> select(first_floor, estimate, conf.low, conf.high) |> as_tibble() |> mutate(mdl = "c_pool", .before = first_floor)



m.ff.c_pool |>
  predictions(
    newdata = datagrid(
      first_floor = c(0, 1))
  )  |>
  mutate(mdl = "c_pool") |>
  pivot_wider(id_cols = mdl, names_from = first_floor, values_from = estimate, names_prefix = "floor = ") |> mutate(n = length(my.counties)) |> uncount(n) |>
  mutate(county = my.counties, .after = mdl) -> c_pool_pred

# tidy(m.ff.no_pool) |> arrange(estimate) |>
#   print(n = 1000)

m.ff.no_pool |>
  predictions(
    newdata = datagrid(
      first_floor = c(0, 1),
      county = my.counties
    )
  ) |> select(county, first_floor, estimate, conf.low, conf.high) |> as_tibble()  |>
  pivot_wider(id_cols = county, names_from = first_floor, values_from = estimate, names_prefix = "floor = ") |> mutate(mdl = "no_pool", .before = county) ->  np_pred



m.ff.prtl_pool |>
  predictions(
    newdata = datagrid(
      first_floor = c(0, 1),
      county = my.counties
    )
  ) |> select(county, first_floor, estimate, conf.low, conf.high) |> as_tibble()  |>
  pivot_wider(id_cols = county, names_from = first_floor, values_from = estimate, names_prefix = "floor = ") |> mutate(mdl = "prtl_pool", .before = county) ->
  partl_pool

library(extrafont)
theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold")) +
    theme(legend.position = "bottom")
}





radon_mn |> filter(county %in% my.counties) |>
  mutate(first_floor = if_else(first_floor == 0, "floor = 0", "floor = 1")) |>
  ggplot(aes(x = first_floor, y = log.radon)) +
  geom_point() +
  geom_segment(
    data = c_pool_pred,
    aes(
      x = "floor = 0",
      xend = "floor = 1",
      y = `floor = 0`,
      yend = `floor = 1`
      ),
    color = "red",
    linetype = "dashed",
    linewidth = 1
    ) +
  geom_segment(
    data = np_pred,
    aes(
      x = "floor = 0",
      xend = "floor = 1",
      y = `floor = 0`,
      yend = `floor = 1`
    ),
    linetype = "dashed"
  ) +
  geom_segment(
    data = partl_pool,
    aes(
      x = "floor = 0",
      xend = "floor = 1",
      y = `floor = 0`,
      yend = `floor = 1`
    ),
    color = "blue"
  ) +
  facet_wrap(~county, scales = "free_y") +
  theme_clean() +
  labs(x = NULL, y = "Log Radon level")  +
  scale_x_discrete(expand=expansion(mult=0.1)) +
  theme(text = element_text(size = 12))

### Group level Uranium
###

md.lm1 <- radon_mn |>
  lm(log.radon ~ u , data = _)
md.lm2 <- radon_mn |>
  lm(log.radon ~ (u + first_floor)^2 + I(u*u), data = _)
modelsummary(list(md.lm1, md.lm2), stars = TRUE)
md.lvl12 <- radon_mn |>
  lmer(log.radon ~ u +  (1|county), data = _)
summary(md.lvl12)

df  <- radon_mn |>
  select(county, u) |> unique() # |> mutate(first_floor = 0)

pred.lm <- predictions(
  md.lm,
  by = "u"
  )

pred.lvl12 <- predictions(
  md.lvl12,
  newdata = df,
  by = "u"
)


pred |>
  ggplot(aes(x = u, xend = u, y = conf.low, yend = conf.high)) +
  geom_segment() +
  geom_point(aes(y = estimate)) +
  geom_line(
    data = pred.lm,
    aes(x = u, y = estimate),
    linetype  = "dashed"
    ) +
  geom_ribbon(
    data = pred.lm,
    aes(x = u, ymin = conf.low, ymax = conf.high),
    alpha  = .2
  )


############################

finches <- read_rds("geospiza.rds")



