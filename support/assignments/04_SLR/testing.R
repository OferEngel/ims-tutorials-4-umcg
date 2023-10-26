# You can either install and load stevemisc, or call this function randomly. Your pick.
cor2data <- function(cor, n, seed){
  # number of observations to simulate
  nobs = n
  # Cholesky decomposition
  U = t(chol(cor))
  nvars = dim(U)[1]
  if(!missing(seed)){
    set.seed(seed)
    }
  # Random variables that follow the correlation matrix
  rdata = matrix(rnorm(nvars*nobs,0,1), nrow=nvars, ncol=nobs)
  X = U %*% rdata
  # Transpose, convert to data
  # require(tidyverse)
  Data = t(X)
  Data = as.data.frame(Data)
  return(Data)
  }

vars = c("x",  "e")
Cor <- matrix(cbind(1, 0.001,
                    0.001, 1), nrow=2)

rownames(Cor) <- colnames(Cor) <- vars
A <- cor2data(Cor, 10000, 8675309) %>% as_tibble() # Jenny I got your number...
A$y <- with(A, 1 + x + .5*e)

library(santoku)

A |> sample_n(size = 200) |>
  ggplot(aes(x, y)) +   geom_point(width = .7) +
  geom_smooth(method="lm", se = FALSE) +
  theme_clean() +   labs(x="", y = "")

A_strata <- A |>
  mutate(
    bins = chop_equally(
      x,
      groups = 15,
      labels = paste("stratum", 1:15))
    ) |> filter(bins != "stratum 1" & bins != "stratum 15")

A_strata |>
  ggplot(aes(bins, y)) +
  geom_jitter(width = .7) +
  geom_boxplot(alpha = .9) +
  labs(x="", y = "") +
  theme_clean()

lm(y ~ x, A) |> summary()

A_sum <- A_strata |>
  summarise(mn.y = mean(y),
            sd.y = sd(y),
            n = n(),
            mn.x = mean(x),
            .by = bins) |>
  mutate(
    y_hat = 1 + 1 * mn.x,
    y.ratio = mn.y / y_hat
    ) |>
  arrange(bins)

A_sum
A_sum |>
  summarise(mn.sd.y = mean(sd.y))
