
library(rstan)
library(dplyr)

setwd("~/Teaching/Stat-506/JAGS-lecture")
dat <- read.csv("clean-baseball.csv") %>% tbl_df()

stan_dat <- with(dat, list(
  N = nrow(dat), 
  J = length(unique(playerID)), 
  j = as.integer(as.factor(playerID)), 
  y = woba, 
  age1 = a1, 
  age2 = a2)
)

model_in_stan <- "
data {
  int<lower = 0> N;
  int<lower = 0> J;
  int<lower=1, upper = J> j[N];
  vector[N] age1;
  vector[N] age2;
  vector[N] y;
}
parameters {
  real beta0;
  real beta1;
  real beta2;
  real<lower = 0, upper = 10> sigma_y;
  real<lower = 0, upper = 10> sigma_b;
  vector[J] b;
}
model {
  vector[N] yhat;
  
  for(i in 1:N)
    yhat[i] <- beta0 + beta1 * age1[i] + beta2 * age2[i] + b[j[i]];

  y ~ normal(yhat, sigma_y);
  b ~ normal(0, sigma_b);
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);
  beta2 ~ normal(0, 100);
}
"

trans <- stanc(model_code = model_in_stan)
comp <- stan_model(stanc_ret = trans, verbose = F)
samps <- rstan::sampling(comp, stan_dat)

traceplot(samps, pars = c("beta0", "beta1", "beta2", "sigma_b", "sigma_y"), inc_warmup = F)
