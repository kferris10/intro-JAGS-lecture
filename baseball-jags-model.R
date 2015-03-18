
library(R2jags)   ## to call JAGS from R
library(plyr)     ## always load plyr before dplyr
library(dplyr)
set.seed(42)

# loading data
setwd("wherever")
dat <- read.csv("clean-baseball.csv") %>% tbl_df()

# JAGS ----------------------------------------------
# saving JAGS model as a .txt file
cat("
model {
  # likelihood
  for(i in 1:N) {
    y[i] ~ dnorm(yhat[i], tau_y)
    yhat[i] <- beta0 + beta1 * age[i] + beta2 * age2[i] + b[player[i]]
  }
  for(j in 1:J) {
    b[j] ~ dnorm(0, tau_b)
  }
  
  # priors
  beta0 ~ dnorm(0, .001)
  beta1 ~ dnorm(0, .001)
  beta2 ~ dnorm(0, .001)
  tau_y <- pow(sigma_y, -2)
  sigma_y ~ dunif(0, 1000)
  tau_b <- pow(sigma_b, -2)
  sigma_b ~ dunif(0, 1000)
}", 
    file = "jags-baseball-model.txt")

# data to pass to jags
jags_dat <- with(dat, list(
  N = nrow(dat), 
  J = length(unique(playerID)), 
  player = as.integer(as.factor(playerID)), 
  y = woba, 
  age = a1, 
  age2 = a2)
)

# initializing parametets - worry about this later
inits <- function() {
  list(b0 = rnorm(1, 0, 1), 
       b1 = rnorm(1, 0, 1), 
       b2 = rnorm(1, 0, 1), 
       sigma_y = runif(1, 0, .1), 
       sigma_b = runif(1, 0, .1))
}

# doing the warmup
warmup <- jags.model("jags-baseball-model.txt", jags_dat, inits = inits(), 
                     n.chains = 4, n.adapt = 1000)

# parameters to obtain from JAGS
parms <- c("beta0", "beta1", "beta2", "sigma_y", "sigma_b")
# sampling after completed warmup
samples <- coda.samples(warmup, parms, n.iter = 1000)

# diagnostic plots
plot(samples)
## it looks like things are mixing, so everything's good

# plotting mean aging curves ------------------------------------
## ask me if you have questions

load("baseball-age-polynomials.RData")  ## loading orthogonal polynomial of age
ages <- seq(20, 35, by = 1)             ## setting up vector to predict from
ages_pred <- predict(poly_age, ages)    ## turning ages into an orthogonal polynomial of order 2
ages_mat <- cbind(1, ages_pred)         ## turning it into a matrix

draws <- samples %>% ldply(data.frame)  ## extracting parameters from JAGS
yhats <- as.matrix(draws[, 1:3]) %*% t(ages_mat)  ## multiplying to get estimated responses

# turning yhats into a data.frame
library(tidyr)
pred_woba <- yhats %>% 
  data.frame() %>% 
  tbl_df() %>% 
  setNames(20:35) %>%             ## changing names of data.frame to 20:35
  mutate(draw = 1:nrow(.)) %>%    ## creating a new variable to denote the simulation
  gather(age, woba, -draw)        ## going to long format

# plotting - definitely use ggplot2 here
library(ggplot2)
qplot(age, woba, data = pred_woba, group = draw, geom = "line", alpha = I(.01))

