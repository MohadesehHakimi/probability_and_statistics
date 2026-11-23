head(airquality)
attach(airquality)

# 1.A
boxplot(Temp ~ Month)

# 1.B
boxplot(Wind ~ Month)
boxplot(Ozone ~ Month)  # Ozone is much higher in the summer!
boxplot(Solar.R ~ Month)

# 1.C
boxplot(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")],
        main = "Comparison of Variable Ranges")

# 1.D
plot(Temp, Ozone)
abline(lm(Ozone ~ Temp), col = "red", lwd = 2)

# 1.E
aq <- na.omit(airquality)
aq$sqrtOzone <- sqrt(aq$Ozone)

plot(aq$Wind, aq$sqrtOzone,
     col = aq$Month)

legend("topright",
       legend = unique(aq$Month),
       col = unique(aq$Month),
       pch = 19,
       title = "Month")


# 1.F
wind <- na.omit(airquality$Wind)

hist(wind,
     probability = T,
     breaks = 10)

curve(dnorm(x,
            mean = mean(wind),
            sd = sd(wind)),
      from = min(wind),
      to = max(wind),
      lwd = 2,
      add = TRUE)

solar <- na.omit(airquality$Solar.R)

hist(solar,
     probability = TRUE,
     breaks = 10)

curve(dnorm(x,
            mean = mean(solar),
            sd = sd(solar)),
      from = min(solar),
      to = max(solar),
      col = "red",
      lwd = 2,
      add = TRUE)

# -------------
# 2.A
lambda <- 5
pop <- rexp(2000, rate = lambda)

hist(pop,
     probability = TRUE,
     breaks = 25)

curve(dexp(x, rate = lambda),
      from = 0,
      to = max(pop),
      col = "red",
      lwd = 2,
      add = TRUE)

# 2.B
s <- 0.1
filtered <- pop[pop > s]
y <- filtered - s

hist(pop,
     probability = TRUE,
     breaks = 30)

hist(y,
     probability = TRUE,
     breaks = 30,
     col = rgb(1, 0, 0, 0.4),
     add = TRUE)

curve(dexp(x, rate = lambda),
      from = 0,
      to = max(pop),
      col = "black",
      lwd = 2,
      add = TRUE)

# -------------
# 3.A
data <- read.csv("adult.csv")
head(data)
attach(data)

# 3.B
income_binary <- ifelse(income == ">50K", 1, 0)

age_group <- cut(
    age,
    breaks = c(15, 25, 35, 45, 55, 65, 100),
    labels = c("16-25", "26-35", "36-45", "46-55", "56-65", "65+")
)

bar_data <- tapply(
    income_binary,
    list(gender, age_group),
    mean
)

barplot(
    bar_data,
    beside = TRUE,
    col = c("skyblue", "orange"),
    ylim = c(0, 1)
)

# 3.C
plot(
    income_binary,
    age,
    col = ifelse(gender == 'Male', 'blue', 'red')
)

# 3.D
age_male <- age[gender == "Male"]
age_female <- age[gender == "Female"]

hist(age_male,
     breaks = 15,
     freq = F,
     ylim = c(0, 0.04),
     )
hist(age_female,
     breaks = 15,
     freq = F,
     col = 'skyblue',
     add = TRUE)

# 3.E
d_male <- density(age_male)
d_female <- density(age_female)

plot(
    d_male,
    col = "blue",
    lwd = 2
)

lines(
    d_female,
    col = "red",
    lwd = 2
)

# 3.F
trend_data <- tapply(
    income_binary,
    list(age_group, gender),
    mean
)
plot(
    trend_data[, "Male"],
    type = "o",
    col = "blue",
    pch = 16,
    ylim = c(0, 1),
    xaxt = "n"
)

axis(1, at = 1:nrow(trend_data), labels = rownames(trend_data))

lines(
    trend_data[, "Female"],
    type = "o",
    col = "red",
    pch = 17
)