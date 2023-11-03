# Natality
library(tidyverse)
library(table1)
# natality.orig <- natality
natality.orig |> select(
  DBWT,
  CIG_REC,
  risks,
  MAGER,
  MRACEHISP,
  DMAR,
  MEDUC,
  PRIORLIVE,
  PRIORDEAD,
  BMI
) |>
  drop_na() |>
  mutate_if(is.character, as.factor) |>
  mutate(MEDUC = fct_recode(MEDUC, lower = "<HS"),
         PRIORLIVE_cat = fct_collapse(as.factor(PRIORLIVE), `4+` = as.character(c(4:8, 10, 11))),
         PRIORDEAD_cat = fct_collapse(as.factor(PRIORDEAD), `1+` = as.character(c(1, 2, 3, 12))),
         MEDUC = fct_collapse(MEDUC, Uni=c("Bachelor", "Adv Degree"))
         ) -> natality

GGally::ggpairs(natality)
# DBWT (birthweight (g)), CIG_REC (smoked during pregnancy), risks (risk factors reported), MAGER (mother’s age), MRACEHISP (mother’s race/ethnicity), DMAR (marital status), MEDUC (mother’s education), PRIORLIVE (prior births now living), PRIORDEAD (prior births now dead), and BMI

# write_rds(natality, "natality.rds")
natality <- read_rds("natality.rds")
cols <- names(natality)[c(1,4, 8)]
natality <- natality |>
  select(-PRIORLIVE, -PRIORDEAD)


natality.ctr <- natality |>
  mutate(across(all_of(cols), scale)) |>
  mutate(across(all_of(cols), ~ .x*2))

natality.ctr |>
  summarize(mn = mean(DBWT), sd = sd(BMI))

dim(natality)

summary(natality)

table1(~DBWT +
       # CIG_REC +
       risks +
       MAGER +
       MRACEHISP +
       DMAR +
       MEDUC +
       PRIORLIVE_cat +
       PRIORDEAD_cat +
       BMI + CIG_REC  | MEDUC, data = natality, overall = FALSE)

natality$PRIORDEAD_cat |> table()


natality |> select(-PRIORLIVE, -PRIORDEAD) -> natality

M0 <- lm(DBWT ~ 1, natality.ctr)
M.FULL <- lm(DBWT ~ . + (risks + MAGER + CIG_REC + DMAR + MEDUC + BMI) * (risks + MAGER + CIG_REC + DMAR + MEDUC + BMI), natality.ctr)
M1 <- lm(DBWT ~ . +
           risks:MEDUC +
           CIG_REC:MAGER +
           MEDUC:BMI, natality.ctr)



M0 <- lm(DBWT ~ 1, natality)
M.FULL <- lm(DBWT ~ ., natality)

M.FULL <- lm(DBWT ~ . + (risks + MAGER + CIG_REC + DMAR + MEDUC + BMI) * (risks + MAGER + CIG_REC + DMAR + MEDUC + BMI), natality)




# 7 risksYes:MEDUCHS     276.     113.        2.45 1.44e- 2
# 8 CIG_RECYes:MAGER     -23.2     10.9      -2.13 3.37e- 2
# 9 MEDUCUni:BMI         -18.1      8.40     -2.15 3.14e- 2

MF1 <- lm(DBWT ~  MAGER +
            CIG_REC +
            CIG_REC:MAGER,
          natality.ctr)

MF0 <- lm(DBWT ~  1,
          natality)
MF1 <- lm(DBWT ~  MAGER + I(MAGER^2) +
            CIG_REC +
            CIG_REC:(MAGER + I(MAGER^2)),
          natality)
summary(MF1)

step(MF0,
     scope = list(upper=MF1),
     direction="both",
     test="Chisq",
     data=natality)



x.seq <- seq(min(natality$MAGER), max(natality$MAGER), length.out = 100)

df <- data.frame(
  MAGER = x.seq) |>
  mutate(
    No = predict(MF1, newdata = data.frame(MAGER = x.seq, CIG_REC = "No")),
    Yes = predict(MF1, newdata = data.frame(MAGER = x.seq, CIG_REC = "Yes"))
    ) |>
  pivot_longer(2:3, names_to = "CIG_REC", values_to = "DBWT")


natality |>
  ggplot(aes(MAGER, DBWT, color = CIG_REC)) +
    geom_jitter(alpha = .3) +
    geom_line(data = df, aes(MAGER, DBWT, color = CIG_REC), linewidth = 2 ) +
    scale_color_manual(values = c("red", "blue")) +
    theme_minimal()



summary(M.FULL)
summary(MF1)
x.ctr <- M.FULL |>
  broom::tidy() |>
  filter(p.value <=0.05)

x <- M.FULL |>
  broom::tidy() |>
  filter(p.value <=0.05)

step(M0,
     scope = list(upper=M.FULL),
     direction="both",
     test="Chisq",
     data=natality)

step(M0,
     scope = list(upper=M.FULL),
     direction="both",
     test="Chisq",
     data=natality.ctr)

step(M0,
     scope = list(upper=M.FULL),
     direction="both",
     data=natality.ctr)
summary(MF1)
anova(M0, MF1, M.FULL)

MF <- lm(DBWT ~ DMAR + MRACEHISP + BMI + risks + PRIORLIVE_cat + CIG_REC + PRIORDEAD_cat, data = natality.ctr)

MF <- lm(DBWT ~ DMAR + MRACEHISP + BMI + risks + PRIORLIVE_cat + CIG_REC + PRIORDEAD_cat + MRACEHISP:CIG_REC, data = natality)

MF <- lm(DBWT ~ DMAR + MRACEHISP + BMI + risks + PRIORLIVE_cat +
           CIG_REC + PRIORDEAD_cat + DMAR:MRACEHISP, # + MRACEHISP:CIG_REC,
         data = natality)

levels(natality$DMAR)
summary(MF)
anova(M0, MF, M.FULL)

plot(MF,which = 3)
augment(MF) |> ggplot(aes(.resid)) + geom_density()
# Does the association between cigarette smoking during pregnancy and birthweight depend on mother’s education?

MF.INT_CIG_REC_BWT <- lm(DBWT ~ DMAR + MRACEHISP + BMI + risks + PRIORLIVE_cat + CIG_REC + PRIORDEAD_cat + MEDUC + MEDUC:CIG_REC,
                         data = natality)
summary(M1)

summary(M1.INT_CIG_REC_BWT)

anova(MF, MF.INT_CIG_REC_BWT)

# Does the association between mother’s BMI and birthweight differ between mothers of different race/ethnicity?
MF.INT_MRACEHISP_REC_BWT <- lm(DBWT ~ DMAR + MRACEHISP + BMI + risks + PRIORLIVE_cat + CIG_REC + PRIORDEAD_cat + MEDUC:MRACEHISP, data = natality)
summary(MF.INT_MRACEHISP_REC_BWT)

anova(MF, MF.INT_MRACEHISP_REC_BWT)


# Does the association between mother’s BMI and birthweight depend on mother’s age?
M1.INT_MAGER_BMI_BWT <- lm(DBWT ~ . + MAGER:BMI, natality)
summary(M1.INT_MAGER_BMI_BWT)

anova(M1, M1.INT_MAGER_BMI_BWT)
############################################
##
##




camp_0_48 |>
  mutate(TG = fct_relevel(TG, "Placebo")) |>
  drop_na(dehumid) ->
  camp_0_48

M0 <- lm(POSFEV0 ~ 1, camp_0_48)
M1 <- lm(POSFEV0 ~ TG, camp_0_48)
M2 <- lm(POSFEV0 ~ TG  + dehumid, camp_0_48)
M3 <- lm(POSFEV0 ~ TG  + dehumid + dehumid:TG, camp_0_48)

summary(M1)
summary(M2)
summary(M3)

anova(M0, M1, M2, M3)
M.FULL <- lm(POSFEV0 ~ ., camp_0_48)
summary(M1)

step(M0,
     scope = list(upper=M.FULL),
     direction="both",
     test="Chisq",
     data=camp_0_48)

