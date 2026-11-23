library(ggplot2)
library(reshape2)

#### 1 ####

calc_t <- function(samples, pop_mean) {
    (mean(samples) - pop_mean) / (sd(samples)/sqrt(length(samples)))
}

sample_t <- function(n, ssize) {
    replicate(
        n,
        {
            sample <- rnorm(ssize, mean = 150, sd = 10)
            calc_t(sample, 150)
        }
    )
}

my_sample <- data.frame(t = sample_t(10000, 2))

ggplot(my_sample) +
    geom_histogram(aes(x = t, y = after_stat(density)),
                   bins = 50,
                   fill = "grey80",
                   color = "black") +
    geom_density(aes(x = t)) +
    xlim(-10, 10)

# Looks like a normal distribution! but, is it?

ggplot(my_sample) +
    geom_density(
        aes(x = t),
        linewidth = 1
    ) +
    stat_function(
        fun = dnorm,
        args = list(mean = 0, sd = 1),
        linetype = "dashed",
        linewidth = 1
    ) +
    xlim(-10, 10)

ns <- c(2, 3, 6, 100)

my_sample <- as.data.frame(
    sapply(ns, function(n) sample_t(10000, n))
)

colnames(my_sample) <- paste0("t", ns)

my_sample$normal <- rnorm(10000)
head(my_sample)

melted_sample <- melt(my_sample)
head(melted_sample)

ggplot(data = melted_sample) +
    geom_density(aes(x = value,
                     color = variable)) +
    xlim(-10, 10) +
    labs(
        title = "Comparison of t distributions for different sample sizes",
        x = "Value",
        y = "Density",
        color = "Distribution"
    )

ggplot() +
    geom_density(data = my_sample, aes(x = t2)) +
    xlim(-10, 10) +
    stat_function(
        fun = dt,
        args = list(df = 1),
        linetype = "dashed",
        linewidth = 1
    )

t <- calc_t(c(130, 120, 136, 145, 150, 140), 150)
t

# H0: the data come from N(150, 10).
# H1: the data don't come from N(150, 10).
p_value <- 2 * pt(-abs(t), df = 5)
p_value

p_emp <- mean(my_sample$t6 < t)
p_emp * 2


#### 2 ####
df <- read.csv('Golf.csv')
head(df)

stat <- mean(df$New - df$Curr)
stat

d <- df$New - df$Current
B <- 10000

d_perm <- replicate(B, {
    signs <- sample(c(-1, 1), length(d), replace = TRUE)
    mean(signs * d)
})

hist(d_perm,
     breaks = 30,
     main = "Permutation Test for Mean Difference",
     xlab = "Mean Difference (New - Old)")

abline(v = stat, col = "red", lwd = 2)

p_value <- mean(d_perm <= stat)
p_value
# can't rule out H0!

classes <- rep(c(1, 2), c(10, 90))
x <- rnorm(100, 0, s=classes)

meandiff <- function(x, classes) {
    mean(x[classes == 1]) - mean(x[classes == 2])
}

meanpermtest <- function(x, classes, n = 1000) {
    observed <- meandiff(x, classes)
    perms <- replicate(n, meandiff(x, sample(classes)))
    mean(abs(observed) > abs(perms))
}

perm.p <- replicate(1000, {
    x <- rnorm(100, 0, s=classes)
    meanpermtest(x, classes)}
    )

table(cut(perm.p, c(0, .05, .1, .5, .9, .95, 1)))

# permutation test fails to give a uniform p-value distribution when
# the two groups come from different distributions, even if their means are equal.