library(tidyverse)
library(lmerTest)
library(broom.mixed)
# Tiles: Linear Mixed effects models I
######################

df <- tribble(
  ~person, ~t0, ~t1, ~t2,
  "p1", 102, 97, 95,
  "p2", 96, 93, 87,
  "p3", 83, 79, 78,
  "p4", 79, 77, 75
)


# to calculate the coefficients shown, just run the following:
df |> pivot_longer(
  2:4,
  names_to = "time",
  values_to = "weight"
) |>
  mutate(time = as.numeric(gsub("t", "", time))) |>
  lm(weight ~ time, data = _) |>
  summary()



df |> pivot_longer(
  2:4,
  names_to = "time",
  values_to = "weight"
) |>
  mutate(time = as.numeric(gsub("t", "", time))) |>
  lm(weight ~ time + person, data = _) |>
  summary()


df |> pivot_longer(
  2:4,
  names_to = "time",
  values_to = "weight"
) |>
  mutate(time = as.numeric(gsub("t", "", time))) |>
  lmerTest::lmer(weight ~ time + (1|person), data = _) |>
  summary()



# Tiles: Linear mixed-effects models part II: random intercepts and slopes
# Try to replicate it!

df <- tribble(
  ~weight,~subject, ~diet, ~week,
  102,1,"A",0,
  96,2,"A",0,
  83,3,"B",0,
  79,4,"B",0,
  97,1,"A",1,
  93,2,"A",1,
  79,3,"B",1,
  77,4,"B",1,
  95,1,"A",2,
  87,2,"A",2,
  78,3,"B",2,
  75,4,"B",2,
  93,1,"A",3,
  85,2,"A",3,
  74,3,"B",3,
  72,4,"B",3
) |>mutate(subject = factor(subject))

M1 <- df |>
  lm(weight ~ week, data = _)
M1 |> summary()
# df.residual(M1)

M2 <- df |>
  lm(weight ~ 0 + week + subject, data = _)

summary(M2)
df.residual(M2)


M2.lmer <- df |>
  lmerTest::lmer(weight ~ week + (1|subject), data = _, REML=FALSE)
summary(M2.lmer)
ranef(M2.lmer)
df.residual(M2.lmer)


broom.mixed::augment(M2.lmer) |>
  inner_join(df, by = c("week", "subject"), suffix = c("", ".y") ) |>
  select(week, subject, .fitted, diet, weight) |>
  ggplot(aes(week, .fitted, group = subject, color = diet)) +
  geom_line() +
  geom_point(aes(y = weight))





M3.lmer <- df |>
  lmerTest::lmer(weight ~ week + (week|subject), data = _, REML = FALSE)
summary(M3.lmer)
ranef(M3.lmer)

broom.mixed::augment(M3.lmer) |>
  inner_join(df, by = c("week", "subject"), suffix = c("", ".y") ) |>
  select(week, subject, .fitted, diet, weight) |>
  ggplot(aes(week, .fitted, group = subject, color = diet)) +
  geom_line() +
  geom_point(aes(y = weight)) +
  geom_line(data = )
  theme(legend.position = "none")

predict(M3.lmer, df) |>
cbind(predict(M3.lmer, re.form = ~week + (1|subject))) |>
  cbind(broom.mixed::augment(M3.lmer)$.fitted)
df |> cbind(y_hat = predict(M3.lmer, re.form = NULL))


M4.lmer <- df |>
  lmerTest::lmer(weight ~ week + diet + (1|subject), data = _, REML = FALSE)
summary(M4.lmer)
ranef(M4.lmer)

# Chick weight

# https://marginaleffects.com/vignettes/lme4/
# library(lme4)
library(tidyverse)
library(patchwork)
library(marginaleffects)
cw <- datasets::ChickWeight

table(cw$Chick)
table(cw$Diet)

## unconditional linear growth model
fit1 <- lmer(
  weight ~ 1 + Time + (1 + Time | Chick),
  data = cw)
summary(fit1)

## conditional quadratic growth model
fit2 <- lmer(
  weight ~ 1 + Time + I(Time^2) + Diet + Time:Diet + I(Time^2):Diet + (1 + Time + I(Time^2) | Chick),
  data = ChickWeight)
summary(fit2)
lmtest::lrtest(fit1, fit2)


# Unit level predictions
# Predict weight of each chick over time:

pred1 <- predictions(fit1,
                     newdata = datagrid(Chick = cw$Chick,
                                        Time = 0:21))
plot_predictions(
  fit1,
  # newdata = datagrid(Chick = cw$Chick),
  condition = list(Time = 0:21, Chick = cw$Chick)) +
  theme(legend.position = "none")


ggplot(pred1, aes(Time, estimate, level = Chick)) +
  geom_line() +
  labs(y = "Predicted weight", x = "Time", title = "Linear growth model")

pred2 <- predictions(
  fit2,
  newdata = datagrid(Chick = ChickWeight$Chick,
  Time = 0:21))

plot_predictions(
  fit2,
  # newdata = datagrid(Chick = cw$Chick),
  condition = list(
    Time = 0:21,
    Chick = cw$Chick,
    Diet = cw$Diet
    ),
  )  +
  facet_wrap(~ Diet, labeller = label_both) +
  theme(legend.position = "none")


plot_predictions(
  fit2,
  # newdata = datagrid(Chick = cw$Chick),
  by = c("Time", "Chick", "Diet"),
)  +
  facet_wrap(~ Diet, labeller = label_both) +
  theme(legend.position = "none")


ggplot(pred2, aes(Time, estimate, level = Chick)) +
  geom_line() +
  labs(y = "Predicted weight", x = "Time", title = "Quadratic growth model")

predictions(fit2) |>
ggplot(aes(Time, estimate, level = Chick)) +
  geom_line() +
  ylab("Predicted Weight") +
  facet_wrap(~ Diet, labeller = label_both)

augment(fit2) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point()



# Population-level predictions
# To make population-level predictions, we set the Chick variable to NA, and set re.form=NA. This last argument is offered by the lme4::predict function which is used behind the scenes to compute predictions:



p1 <- plot_predictions(fit2,
                 condition = list(
                   Time = 0:21,
                   Diet = 1:4
                   ),
                 re.form = NA) +
  facet_wrap(~Diet)

p2 <- predictions(
  fit2,
  newdata = datagrid(Chick = NA,
                     Diet = 1:4,
                     Time = 0:21),
  re.form = NA) |>
ggplot(aes(x = Time, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = .1, fill = "red") +
  geom_line() +
  facet_wrap(~ Diet, labeller = label_both) +
  labs(title = "Population-level trajectories")

p1 + p2



library(faux)

# Creates random factors
add_random(subj = 4, item = 2)

# Adding within factors
# For each factor combination assign *both* pre and post
add_random(subj = 4, item = 2) %>%
  add_within("subj", time = c("pre", "post"))


# Adding between factors
# Subject allocate only *one* between-factor
add_random(subj = 4, item = 2) %>%
  add_between("subj", cond = c("ctrl", "treatment"))




# define parameters
subj_n = 10  # number of subjects
item_n = 10  # number of items
b0 = 1.2       # intercept
b1 = 1.4       # fixed effect of condition
u0s_sd = 1   # random intercept SD for subjects
u0i_sd = 1   # random intercept SD for items
u1i_sd = 1   # random b1 slope SD for items
r01i = 0     # correlation between random effects 0 and 1 for items
sigma_sd = .5 # error SD


# Y_si = b0 + b0_s + b0_i + (b1 + b1_i)*X_cond + epsilon
# b0_s ~ N(0, u0s_sd^2)
# b0_i ~ N(0, u0i_sd^2)
# b1_i ~ N(0, u1i_sd^2)
# epsilon ~ N(0, sigma_sd^2)

M.lmer <- df |>
  lmerTest::lmer(dv ~ cond.t + (1|subj) + (cond.t|item), data = _)
summary(M.lmer)


# set up data structure
df <- add_random(subj = subj_n, item = item_n) %>%
  # add and recode categorical variables
  add_between("subj", cond = c("control", "test")) %>%
  add_recode("cond", "cond.t", control = 0, test = 1) %>%
  # add random effects
  add_ranef("subj", u0s = u0s_sd) %>%
  add_ranef("item", u0i = u0i_sd, u1i = u1i_sd, .cors = r01i) %>%
  add_ranef(sigma = sigma_sd) %>%
  # calculate DV
  mutate(dv = b0 + u0s + u0i + (b1 + u1i) * cond.t + sigma)


# Y_si = b0 + b0_s + b0_i + (b1 + b1_i)*X_cond + epsilon
# b0_s ~ N(0, u0s_sd^2)
# b0_i ~ N(0, u0i_sd^2)
# b1_i ~ N(0, u1i_sd^2)

# define parameters
subj_n = 6  # number of subjects
b0 = 2.6     # intercept
b1 = 3.2   # fixed effect of diet
b2 = -0.6     # fixed effect of weeks
u0s_sd = 1.5   # random intercept SD for subjects
sigma_sd = .5 # error SD


# Y_s  = b0_s + b_1*X_diet + b_2*X_week +  epsilon
# b0_s = b_0  + u0s,  u0s ~ N(0, u0s_sd^2)


# set up data structure
df <- add_random(subj = subj_n) %>%
  # add and recode diet variables
  add_between("subj", diet = c("diet A", "diet B")) |>
  add_contrast("diet", "treatment", colnames = "diet.B") |>
  add_within("subj", week = 1:4) %>%
  # add random effects
  add_ranef("subj", u0s = u0s_sd) %>%
  add_ranef(sigma = sigma_sd) %>%
  # calculate DV
  mutate(weight = b0 + u0s + b2*week + b1 * diet.B + sigma)

df |>
  lmerTest::lmer(weight ~ diet.B + week + (1|subj), data = _) |>
  summary()


df |>
  ggplot(aes(week, weight, color = diet, group = subj)) +
  geom_point() +
  geom_line()

tibble(
  dam.prob = rbeta(
    n = 10000,
    shape1 = .5,
    shape2 = .5)
) |>
  ggplot() +
  geom_histogram(
    aes(dam.prob, y=after_stat(density)),
    binwidth = 0.09
  ) +
  geom_function(
    fun = dbeta,
    args=list(shape1 = .5, shape2 =.5),
    size = 1.5, color = "red", alpha = .8) +
  scale_x_continuous(limits = c(-.05, 1.05))

library(tidyverse)
library(broom)
library(faux)
n.dams <- 24
n.pups <- 10
scenario.1a <- add_random(dam = n.dams, pup = n.pups) |>
  mutate(
    p.def = 0.5,
    defect = rbinom(n.dams * n.pups, 1, p.def),
    scenario = "1.a"
  )


scenario.1a |> summarise(n_defect = sum(defect), .by = dam) |>
  glm(cbind(n_defect, n.pups - n_defect) ~ 1, data = _, family = binomial) |>
  summary()

scenario.1a |> summarise(n_defect = sum(defect), .by = dam) |>
  glm(cbind(n_defect, n.pups - n_defect) ~ 1, data = _, family = quasibinomial) |>
  summary()

scenario.1b <- add_random(dam = n.dams, pup = n.pups) |>
  mutate(
    p.def = rep(rbeta(n=n.dams, .5, .5), each = n.pups),
    defect = rbinom(n.dams * n.pups, 1, p.def),
    scenario = "1.b"
  )

scenario.1b |> summarise(n_defect = sum(defect), .by = dam) |>
  glm(cbind(n_defect, n.pups - n_defect) ~ 1, data = _, family = binomial) |>
  summary()

scenario.1b |> summarise(n_defect = sum(defect), .by = dam) |>
  glm(cbind(n_defect, n.pups - n_defect) ~ 1, data = _, family = quasibinomial) |>
  summary()

scenario.1a |>
  add_row(scenario.1b) |>
  summarise(
    n_defects = sum(defect),
    .by = c(scenario, dam)
  ) |>
  ggplot(aes(n_defects)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~scenario, scales = "free")




reprex::reprex()



read_csv("https://bit.ly/3Rpo00j") |>
  select(-1)  |>
  mutate(id = factor(id)) |>
  ggplot(aes(na, perform_type)) +
  geom_point() +
  facet_wrap(~id)

md |> summarise(na = mean(na), .by = id) |>
  ggplot(aes(na)) +
  geom_histogram(binwidth = 2)






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

## https://m-clark.github.io/posts/2019-05-14-shrinkage-in-mixed-models/


library(tidyverse)
library(broom)
library(faux)
library(lmerTest)

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
library(tidyverse)
library(lmerTest)
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

md.lm <- radon_mn |>
  lm(log.radon ~ u , data = _)

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

    )

############################

finches <- read_rds("geospiza.rds")



