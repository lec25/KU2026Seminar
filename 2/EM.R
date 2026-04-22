mu <- 2
tol <- 1e-6
diff <- Inf

while (diff > tol) {
  mu_new <- (0 + 2 + mu) / 3
  diff <- abs(mu_new - mu)
  mu <- mu_new
  print(mu_new)
}



mu <- 2
tol <- 1e-6
diff <- Inf
while (diff > tol) {
  
  EMl = function(mu2) integrate(
    {function(x) (dnorm(0, mu2, 1, log = TRUE)+
      dnorm(2, mu2, 1, log = TRUE)+
      dnorm(x, mu2, 1, log = TRUE)) *
        dnorm(x, mu, 1)}, -Inf, Inf)$value
  
  mu_new = optimize(EMl, c(-100, 100), maximum = TRUE)$maximum
  diff <- abs(mu_new - mu)
  mu <- mu_new
  print(mu_new)
}

## Eg 2
y <- c(-0.39, 0.12, 0.94, 1.67, 1.76, 2.44, 3.72, 4.28, 4.92, 5.53, 
       0.06, 0.48, 1.01, 1.68, 1.80, 3.25, 4.12, 4.60, 5.28, 6.22)

hist(y)

## MLE
ll <- function(par){
  
  pi <- par[1]
  mu1 <- par[2]
  mu2 <- par[3]
  sigma1 <- par[4]
  sigma2 <- par[5]
  
  f1 <- dnorm(y, mu1, sigma1)
  f2 <- dnorm(y, mu2, sigma2)
  
  -sum(log((1 - pi)*f1 + pi*f2))
}

optim(c(0.5, 0.5, 1, 0.5, 1),ll,method="L-BFGS-B",
      lower = c(1e-4, -Inf, -Inf, 1e-4, 1e-4),
      upper = c(1 - 1e-4, Inf, Inf, Inf, Inf))
  


## EM
par <- c(0.5, 0.5, 1, 0.5, 1)  # (pi, mu1, mu2, sigma1, sigma2)
tol <- 1e-6
diff <- Inf

while (diff > tol) {
  
  pi <- par[1]
  mu1 <- par[2]
  mu2 <- par[3]
  sigma1 <- par[4]
  sigma2 <- par[5]
  
  ## ---- E-step ----
  f1 <- dnorm(y, mu1, sigma1)
  f2 <- dnorm(y, mu2, sigma2)
  
  gamma <- pi * f2 / ((1 - pi) * f1 + pi * f2)
  
  ## ---- M-step ----
  pi_new <- mean(gamma)
  
  mu1_new <- sum((1 - gamma) * y) / sum(1 - gamma)
  mu2_new <- sum(gamma * y) / sum(gamma)
  
  sigma1_new <- sqrt(sum((1 - gamma) * (y - mu1_new)^2) / sum(1 - gamma))
  sigma2_new <- sqrt(sum(gamma * (y - mu2_new)^2) / sum(gamma))
  
  par_new <- c(pi_new, mu1_new, mu2_new, sigma1_new, sigma2_new)
  
  diff <- max(abs(par_new - par))
  par <- par_new
  
  print(par)
}

## EM --> incomplete data MLE
