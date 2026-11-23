library(ggplot2)

#### 1 ####
# A
simulate_series <- function(p) {
    wins_team1 <- 0
    wins_team2 <- 0
    
    while (wins_team1 < 4 && wins_team2 < 4) {
        game_result <- rbinom(1, 1, p)
        
        if (game_result == 1) {
            wins_team1 <- wins_team1 + 1
        } else {
            wins_team2 <- wins_team2 + 1
        }
    }
    
    return(wins_team1 == 4)
}

replicate(1000, simulate_series(0.6))

# B
ps <- seq(0.5, 1, length.out = 100)

all_probs <- sapply(ps, function(p) {
    mean(replicate(1000, simulate_series(p)))
})

all_probs

df <- data.frame(
    p = ps,
    probs = all_probs
)
ggplot(df, aes(x = p, y = probs)) + geom_line() + geom_point()

# C
simulate_series_n <- function(p, n) {
    w1 <- 0
    w2 <- 0
    while (w1 < n && w2 < n) {
        game <- rbinom(1, 1, p)
        if (game == 1) w1 <- w1 + 1 else w2 <- w2 + 1
    }
    (w1 == n)
}

estimate_lose_prob <- function(p, n, B = 5000) {
    1 - mean(replicate(B, simulate_series_n(p, n)))
}

p <- 0.6
ns <- 1:20
B <- 5000

df1 <- data.frame(
    n = ns,
    lose_prob = sapply(ns, function(n) estimate_lose_prob(p, n, B))
)

ggplot(df1, aes(x = n, y = lose_prob)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = ns)

# D
simulate_series_home_away <- function(p_home, p_away) {
    wins1 <- 0
    wins2 <- 0
    game <- 1
    
    home_games <- c(1, 2, 5, 7)
    
    while (wins1 < 4 && wins2 < 4 && game <= 7) {
        
        if (game %in% home_games) {
            p <- p_home
        } else {
            p <- p_away
        }
        
        result <- rbinom(1, 1, p)
        
        if (result == 1) {
            wins1 <- wins1 + 1
        } else {
            wins2 <- wins2 + 1
        }
        
        game <- game + 1
    }
    
    return(wins1 == 4)
}

estimate_series_prob <- function(p_home, p_away, B = 5000) {
    mean(replicate(B, simulate_series_home_away(p_home, p_away)))
}

p_away <- 0.55
p_homes <- seq(0.55, 0.75, by = 0.02)

df <- data.frame(
    p_home = p_homes,
    series_prob = sapply(p_homes, function(ph)
        estimate_series_prob(ph, p_away, B = 5000))
)

ggplot(df, aes(x = p_home, y = series_prob)) +
    geom_line() +
    geom_point()

#### 2 ####
# A
x <- rexp(1000, rate = 0.4)
df <- data.frame(x = x)

ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30,
                   fill = "lightblue",
                   color = "black") +
    geom_density(size = 1, color = "red")

# B
# P(X > t)
t <- 2
P_t <- length(df$x[x > t]) / length(df$x)


# P(X > s + t|X > s)
s <- 3
more_than_s <- data.frame(x = df$x[x > s])
more_than_st <- more_than_s$x[x > s + t]
P_st <- length(df$x[df$x > s + t]) / length(df$x[df$x > s])


more_than_s <- more_than_s - s
more_than_s <- data.frame(x = more_than_s)

ggplot() +
    geom_histogram(
        data = df,
        aes(x = x, y = after_stat(density)),
        bins = 30,
        fill = "lightblue",
        color = "black",
        alpha = 0.4
    ) +
    geom_density(
        data = df,
        aes(x = x),
        color = "blue",
        size = 1
    ) +
    geom_density(
        data = more_than_s,
        aes(x = x),
        color = "red",
        size = 1
    )

# C
x <- matrix(rexp(10000 * 10, rate = 0.4), ncol = 10)
T10 <- rowSums(x)

df <- data.frame(x = T10)

t <- 2
P_t <- length(df$x[df$x > t]) / length(df$x)

s <- 3
more_than_s <- data.frame(x = df$x[df$x > s])

more_than_st <- more_than_s$x[more_than_s$x > s + t]
P_st <- length(more_than_st) / length(more_than_s$x)

more_than_s <- more_than_s - s
more_than_s <- data.frame(x = more_than_s)

ggplot() +
    geom_histogram(
        data = df,
        aes(x = x, y = after_stat(density)),
        bins = 30,
        fill = "lightblue",
        color = "black",
        alpha = 0.4
    ) +
    geom_density(
        data = df,
        aes(x = x),
        color = "blue",
        size = 1
    ) +
    geom_density(
        data = more_than_s,
        aes(x = x),
        color = "red",
        size = 1
    )


