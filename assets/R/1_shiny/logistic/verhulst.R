library(ggplot2)
library(dplyr)

ver <- read.table(
  "http://pbil.univ-lyon1.fr/R/donnees/VerhulstPF1847.txt",
  sep = "\t",
  header = TRUE
) |>
  mutate(obs = obs / 10^6, theo = theo / 10^6)

t0 <- ver$t[1]
N0 <- ver$obs[1]

# Model equations
logistic <- function(t, p) {
  with(as.list(p), {
    K / (1 + ((K - N0) / N0) * exp(-r * (t - t0)))
  })
}

# Minimize
sce <- function(p) {
  theo <- logistic(ver$t, p = setNames(p, c("r", "K")))
  sum((ver$obs - theo)^2)
}

# Fit: K = 5 means 5 millions people max (*10^6)
nlmfit <- nlm(f = sce, p = c(0.02, 5))

tseq <- seq(from = 1800, to = 2000, by = 1)
res <- data.frame(
  t = tseq,
  y = logistic(tseq, p = setNames(nlmfit$estimate, c("r", "K")))
)

lm_coef <- coef(lm(ver$obs ~ ver$t))

ggplot(data = res) +
  geom_line(mapping = aes(x = t, y = y), color = "red") +
  geom_point(
    data = ver,
    mapping = aes(x = t, y = obs)
  ) +
  geom_abline(
    slope = lm_coef[2],
    intercept = lm_coef[1],
    color = "grey",
    linetype = "dashed"
  ) +
  labs(
    x = "Year",
    y = "Population in million",
    title = "Population growth in belgium"
  )


## NLS tools ##
library(nlstools)
ver2 <- read.table(
  "http://pbil.univ-lyon1.fr/R/donnees/VerhulstPF1847.txt",
  sep = "\t",
  header = TRUE
) |>
  mutate(N = obs / 10^6) |>
  select(t, N)
formulaExp <- as.formula(N ~ K / (1 + ((K - N0) / N0) * exp(-r * (t - t0))))
preview(formulaExp, ver2, list(K = 5, r = 0.02, N0 = ver2$N[1], t0 = ver2$t[1]))
nls_res <- nls(
  formulaExp,
  start = list(K = 5, r = 0.02, N0 = ver2$N[1], t0 = ver2$t[1]),
  data = ver2
)
overview(O2K.nls1)
plotfit(O2K.nls1, smooth = TRUE)
