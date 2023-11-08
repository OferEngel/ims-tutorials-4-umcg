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
