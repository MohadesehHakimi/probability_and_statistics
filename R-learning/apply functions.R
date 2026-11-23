# lapply / sapply
x <- data.frame(a = 1:10, b = rnorm(10))
lapply(x, mean)

set.seed(10)
x <- 1:4
lapply(x, runif, min = 0, max = 10)

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
y <- lapply(x, mean)
z <- sapply(x, mean)

# apply
x <- cbind(a = 3, b = c(4:1, 2:5))
apply(x, 2, sum)
apply(x, 1, sum)

# mapply
mapply(rnorm, 1:5, 1:5, 2)

# tapply
