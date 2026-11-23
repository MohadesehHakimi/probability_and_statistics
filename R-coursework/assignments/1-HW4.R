# 1.A
df <- read.csv('Clt-data.csv')
dim(df)
head(df, 10)

# 1.B
m <- mean(df$Wall.Thickness)
m
hist(df$Wall.Thickness,
     main = "Histogram of Wall Thickness",
     xlab = "Wall Thickness")

# 1.C
means <- replicate(
    9000,
    mean(sample(df$Wall.Thickness, size = 30))
)
hist(means)

# 2.A
mysample <- function() {
    n <- 2^23
    (sample.int(n, 1) - 1) / n
}

# 2.B
x <- replicate(100000, mysample())
hist(x)
mean(x)

# 2.C
myrexp <- function(lambda = 1) {
    u <- mysample()
    -log(u) / lambda
}

n <- 10000
lambdas <- c(0.5, 1, 2)

library(ggplot2)

df <- do.call(rbind, lapply(lambdas, function(lam) {
    data.frame(
        value = c(
            replicate(n, myrexp(lam)),
            rexp(n, rate = lam)
        ),
        source = rep(c("myrexp", "rexp"), each = n),
        lambda = factor(lam)
    )
}))

ggplot(df, aes(x = value, color = source, fill = source)) +
    geom_density(alpha = 0.3) +
    facet_wrap(~ lambda, scales = "free") +
    labs(
        title = "Comparison of Exponential Random Number Generators",
        subtitle = "Inverse Transform Sampling vs rexp",
        x = "x",
        y = "Density"
    ) +
    theme_minimal()

# 2.D
erfinv <- function(x) {
    w <- -log((1 - x) * (1 + x))
    if (w < 5.000000) {
        w <- w - 2.500000
        p <- 2.81022636e-08
        p <- 3.43273939e-07 + p * w
        p <- -3.5233877e-06 + p * w
        p <- -4.39150654e-06 + p * w
        p <- 0.00021858087 + p * w
        p <- -0.00125372503 + p * w
        p <- -0.00417768164 + p * w
        p <- 0.246640727 + p * w
        p <- 1.50140941 + p * w
    } else {
        w <- sqrt(w) - 3.000000
        p <- -0.000200214257
        p <- 0.000100950558 + p * w
        p <- 0.00134934322 + p * w
        p <- -0.00367342844 + p * w
        p <- 0.00573950773 + p * w
        p <- -0.0076224613 + p * w
        p <- 0.00943887047 + p * w
        p <- 1.00167406 + p * w
        p <- 2.83297682 + p * w
    }
    return(p * x)
}

mynorm <- function(mu = 0, sigma = 1) {
    u <- mysample()
    z <- erfinv(2 * u - 1)
    mu + sigma * sqrt(2) * z
}

x <- replicate(10000, mynorm(mu = 2, sigma = 1.5))
r <- rnorm(10000, 2, 1.5)
df <- data.frame(x, r)
ggplot() +
    geom_density(aes(x = x, color = "mynorm"), data = df) +
    geom_density(aes(x = r, color = "rnorm"), data = df) +
    theme_minimal()

