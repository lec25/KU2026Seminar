x1 <- 2
tol <- 1e-6
diff <- Inf

while (diff > tol) {
  x2 <- (0 + 2 + x1) / 3
  # x2 <- var(c(0,2,x1))
  diff <- abs(x2 - x1)
  x1 <- x2
  print(x1)
}

x1

x <- 0
tol <- 1e-6

repeat {
  x_new <- var(c(0,2,x))
  if (abs(x_new - x) < tol) break
  x <- x_new
}

x