#### 1 ####
library(ggplot2)

n <- 50000
u1 <- runif(n)
u2 <- runif(n)

X <- data.frame(val = sqrt(-2 * log(u1)) * cos(2 * pi * u2))
Y <- data.frame(val = sqrt(-2 * log(u1)) * sin(2 * pi * u2))

ggplot(X) +
    geom_histogram(aes(x = val))

ggplot(Y) +
    geom_histogram(aes(x = val))

both <- data.frame(
    X = X$val,
    Y = Y$val
)
ggplot(both) +
    geom_point(aes(x = X, y = Y), alpha = 0.15)

cor(both$X, both$Y) # almost zero
mean(both$X)
var(both$X)

mean(both$Y)
var(both$Y)

Z <- rnorm(n, 0, 1)

df_compare <- data.frame(
    value = c(both$X, both$Y, Z),
    variable = factor(rep(c("X", "Y", "Z"), each = n))
)

ggplot(df_compare, aes(x = value, color = variable)) +
    geom_density(linewidth = 1)
# X, Y and Z are all normal distributions!

R <- X$val^2 + Y$val^2
mean(R)

df_RX <- data.frame(
    R = R,
    X = both$X
)

ggplot(df_RX, aes(x = X, y = R)) +
    geom_point(alpha = 0.15) +
    theme_minimal()

cor(df_RX$R, df_RX$X)
# cor = 0 but dependent

all <- sapply(1:5, function(d) rchisq(n, df = d))
df_plot <- data.frame(
    value = c(as.vector(all), R),
    df = factor(c(rep(1:5, each = n), rep('R', n)))
)
head(df_plot)
ggplot(df_plot, aes(x = value, color = df)) +
    geom_density() +
    theme_minimal()
