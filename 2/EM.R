x1 <- 5
tol <- 1e-6
diff <- Inf

while (diff > tol) {
  x2 <- (0 + 2 + x1) / 3
  diff <- abs(x2 - x1)
  x1 <- x2
}

x1

x <- 0
tol <- 1e-6

repeat {
  x_new <- (2 + x) / 3
  if (abs(x_new - x) < tol) break
  x <- x_new
}

x