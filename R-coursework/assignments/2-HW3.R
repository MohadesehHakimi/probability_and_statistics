library(ggplot2)

#### 1 ####
df <- read.csv('data.csv')
head(df)
summary(df)

# A
df$X <- cut(
    df$record_max_temp, 
    breaks = c(-Inf, 70, 100, Inf),
    labels = c('X3', 'X2', 'X1'),
    right = T,
    lowest.included = T
    )

df$Y <- cut(
    df$record_min_temp,
    breaks = c(-Inf, 20, 40, Inf),
    labels = c("Y3", "Y2", "Y1"),
    right = T,
    include.lowest = T
)

joint_counts <- table(df$Y, df$X)
joint_counts

joint_prob <- as.matrix(100 * prop.table(joint_counts))
joint_prob

# B
x1_marginal <- sum(joint_prob[, 3])
x1_marginal

y3_marginal <- sum(joint_prob[1, ])
y3_marginal

# C
ggplot(df) +
    geom_histogram(aes(x = actual_min_temp), bins = 30)

ggplot(df) +
    geom_point(aes(x = actual_min_temp, y = actual_max_temp))


#### 2 ####
n <- 10000

x <- replicate(
    n,
    sort(runif(15))
)

x <- t(x)
dim(x)

a <- x[, 1]
b <- x[, 10]
c <- x[, 15]

a_beta <- rbeta(n, shape1 = 1, shape2 = 15)
b_beta <- rbeta(n, shape1 = 10, shape2 = 6)
c_beta <- rbeta(n, shape1 = 15, shape2 = 1)

mean(a_beta)
1 / 16
mean(b_beta)
10 / 16

df_a <- data.frame(
    value = c(a, a_beta),
    source = factor(
        c(rep("Empirical (a)", length(a)),
          rep("Beta(1,15)", length(a_beta)))
    )
)
ggplot(df_a, aes(x = value, color = source)) +
    geom_density()
    
