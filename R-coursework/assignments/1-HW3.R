# 1.A
makeExp <- function(x, lambda) {
    return (-(1 / lambda) * log(x))
}

# 1.B
sample <- runif(50000)
hist(sample)

s2 <- makeExp(sample, 4)
hist(s2)

# 1.C
lambda <- 4
n <- 10
t <- 0.3
p <- 1 - exp(-lambda * t)

one_binom <- function(exp_samples, n, t) {
    x <- sample(exp_samples, n, replace = TRUE)
    sum(x <= t)
}


binom_sample <- replicate(10000,
                          one_binom(s2, n, t))


hist(binom_sample,
     breaks = seq(-0.5, n + 0.5, 1),
     probability = TRUE,
     main = paste("Binomial(n =", n, ", p =", round(p,3), ")"),
     xlab = "k")

points(0:n, dbinom(0:n, n, p),
       col = "red", pch = 16)

# ----------

# 2.A
df <- read.csv("data.csv")
head(df)

# 2.B
plot(df$time, df$recieve1, type = 'l')
lines(df$time, df$receive2, col = 2)
lines(df$time, df$receive3, col = 3)

# 2.C
attach(df)
df$sendNorm <- (send - mean(send)) / sd(send)
df$r1Norm <- (recieve1 - mean(recieve1)) / sd(recieve1)
df$r2Norm <- (receive2 - mean(receive2)) / sd(receive2)
df$r3Norm <- (receive3 - mean(receive3)) / sd(receive3)

# 2.D
mean(df$sendNorm)
var(df$sendNorm)

mean(df$r1Norm)
var(df$r1Norm)

mean(df$r2Norm)
var(df$r2Norm)

mean(df$r3Norm)
var(df$r3Norm)

# 2.E
plot(df$time, df$r1Norm, type = 'l')
lines(df$time, df$r2Norm, col = 2)
lines(df$time, df$r3Norm, col = 3)

# 2.F
library(ggplot2)

p1 <- ggplot(df, aes(x = send, y = r1Norm)) +
    geom_point(alpha = 0.3) +
    stat_ellipse(level = 0.95, color = "red")
p1

p2 <- ggplot(df, aes(x = send, y = r2Norm)) +
    geom_point(alpha = 0.3) +
    stat_ellipse(level = 0.95, color = "red")

p3 <- ggplot(df, aes(x = send, y = r3Norm)) +
    geom_point(alpha = 0.3) +
    stat_ellipse(level = 0.95, color = "red")
p2
p3

library(ggExtra)

ggMarginal(p1, type = "density")
ggMarginal(p2, type = 'density')
ggMarginal(p3, type = 'density')

# 2.G
cov_sr1 <- cov(df$sendNorm, df$r1Norm)
cov_sr2 <- cov(df$sendNorm, df$r2Norm)
cov_sr3 <- cov(df$sendNorm, df$r3Norm)

cov_sr1
cov_sr2
cov_sr3
