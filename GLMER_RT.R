library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(statmod)
library(fitdistrplus)
library(tidyselect)

df <- readRDS("data/logfiles_metadata_2025-03-06.rds")

glmer_df <- df[,c("PPID","block","word_type","rt","order")] |>
  filter(rt > 250) |>
  group_by(PPID)|>
  mutate(z_rt_pp = (rt - mean(rt))/sd(rt)) |>
  filter(z_rt_pp < 2.5) |>
  ungroup() |>
  mutate(
    participant = as.factor(PPID),
    block = as.factor(block)
  )



## Use this going forward.
glmer_fit <- glmer(
  rt ~ block * word_type + (1|participant),
  data = glmer_df,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)


q <- list(
  invgauss = fitdist(glmer_df$rt, distr = "invgauss",
                     start = list(mean=700, dispersion=300, shape=1)),
  gamma = fitdist(glmer_df$rt, dist = "gamma"),
  normal = fitdist(glmer_df$rt, dist = "norm")
)
ix <- seq(250, 10000, length.out = 1000)

plot(density(glmer_df$rt), lwd = 3, main = NA)
points(
  ix,
  dinvgauss(
    ix,
    mean = q$invgauss$estimate["mean"],
    shape = q$invgauss$estimate["shape"],
    dispersion = q$invgauss$estimate["dispersion"]
  ),
  col = "red", type = "l", lwd = 3
)
points(
  ix,
  dgamma(
    ix,
    shape = q$gamma$estimate["shape"],
    rate = q$gamma$estimate["rate"]
  ),
  col = "blue", type = "l", lwd = 3
)
points(
  ix,
  dnorm(
    ix,
    mean = q$normal$estimate["mean"],
    sd = q$normal$estimate["sd"]
  ),
  col = "green", type = "l", lwd = 3
)
legend(
  "topright",
  legend = c("estimated density", "inverse Gaussian", "gamma", "normal"),
  col = c("black", "red", "blue", "green"),
  lwd = 3
)


