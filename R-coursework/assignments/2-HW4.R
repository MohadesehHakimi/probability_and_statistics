library(dplyr)
library(ggplot2)
#### 1 ####
df <- read.csv("AB_test.csv", header=TRUE)
head(df)
colnames(df)

# A
# H0: no difference between A & B
# H1: a significant difference between A & B

# B
df_A <- df[df$algorithm == 'A', ]
df_B <- df[df$algorithm == 'B', ]

df_A <- df_A[order(df_A$X), ]
df_B <- df_B[order(df_B$X), ]

diff <- df_A$clicks_number - df_B$clicks_number
diff

num <- mean(diff)
s <- sd(diff)
n <- length(diff)
t <- num * sqrt(n) / s
t
p <- 2 * pt(abs(t), df = n - 1, lower.tail = FALSE)
p < 0.05    # False. Can't reject H0.
p           # 0.152307

# using package
t.test(diff)

# C
n <- 1000
df[df$algorithm == 'B', ]$X <- df[df$algorithm == 'B', ]$X - 15

df_wide <- df %>%
    pivot_wider(
        names_from = algorithm,
        values_from = clicks_number
    )
df_wide

obs_diff <- mean(df_wide$A - df_wide$B)

n_perm <- 1000

perm_stats <- replicate(
    n_perm,
    mean(sample(c(-1, 1), nrow(df_wide), replace = TRUE) *
             (df_wide$A - df_wide$B))
)

p_perm <- mean(abs(perm_stats) >= abs(obs_diff))
p_perm      # 0.147
    
    
#### 2 ####
v <- read.csv('vaccination.csv')
head(v)
# A
v$age_group <- cut(v$age, 
                   c(-Inf, 18, 40, 70, Inf), 
                   labels = c('teenager', 'young', 'adult', 'senior'),
                   right = F)
head(v)

table(v$age_group, v$vaccine_type)

ggplot(v) +
    geom_bar(aes(x = age_group, fill = vaccine_type), position = 'dodge') +
    theme_light()
        
# B
# use chi-square test
# H0: age group and vaccination type are independent.
# H1: age group and vaccination type are not independent.

data <- matrix(table(v$age_group, v$vaccine_type), 
               nrow = 4)
data
rownames(data) <- c('teenager', 'young', 'adult', 'senior')
colnames(data) <- c('a', 'b', 'c', 'd', 'e')
data
# library
chisq.test(data)    # p = 0.005538

# manual
row_totals <- rowSums(data)
col_totals <- colSums(data)
grand_total <- sum(data)

outer(row_totals, col_totals)
expected <- outer(row_totals, col_totals) / grand_total
expected

chi_square_stat <- sum((data - expected)^2 / expected)
r <- nrow(data)
c <- ncol(data)
df <- (r - 1) * (c - 1)

p_value <- 1 - pchisq(chi_square_stat, df)
print(p_value)      # p = 0.005538

#### 3 ####
bessel_vals <- numeric(999)
mle_vals <- numeric(999)

for (i in 2:1000) {
    x <- rnorm(i, 2, 2)
    
    bessel_vals[i - 1] <- var(x)
    mle_vals[i - 1] <- sum((x - mean(x))^2) / length(x)
}

df <- data.frame(bessel = bessel_vals, mle = mle_vals)
head(df)

n_vals <- 2:1000
df$n <- n_vals
df$diff <- df$bessel - df$mle

ggplot(df, aes(x = n, y = diff)) +
    geom_line()

ggplot(df, aes(x = mle, y = bessel)) +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0, color = "red")

