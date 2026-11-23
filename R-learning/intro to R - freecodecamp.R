library(pacman)

pacman::p_load(pacman, ggplot2)

library(datasets)
# -----------
head(iris)
plot(iris$Species)
plot(iris$Sepal.Length)
plot(iris$Species, iris$Sepal.Length)
plot(iris)

plot(exp, 1, 5)

# -----------
head(mtcars)
dim(mtcars)

cylinders <- table(mtcars$cyl)
barplot(cylinders)
plot(cylinders)

hist(iris$Sepal.Length)
par(mfrow=c(3, 1))
hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 3),
     breaks = 9)

par(mfrow=c(1, 1))

# -----------
hist(mtcars$wt)
plot(mtcars$wt, mtcars$mpg)

# ----overlay plots----
hist(lynx)
hist(lynx,
     breaks = 14,
     freq = FALSE)
curve(dnorm(x, mean=mean(lynx), sd=sd(lynx)),
      add = TRUE)

# ----basic stats----
summary(iris$Species)
summary(iris$Sepal.Length)
summary(iris)

p_load(psych)
# use describe for quant vars
describe(iris$Sepal.Length)
describe(iris)

# ----selecting cases----
# by category
hist(iris$Petal.Length[iris$Species == "versicolor"])
# by value
hist(iris$Petal.Length[iris$Petal.Length < 2])

hist(iris$Petal.Length[iris$Petal.Length < 5 & iris$Species == "virginica"])
# create subsample: data[rows, cols]
i.setosa <- iris[iris$Species == "setosa", ]

# ----data types----
n1 <- 15
n1
typeof(n1)

n2 <- 1.5
typeof(n2)

c <- "c"
typeof(c)

s <- "a string"
typeof(s)

l <- T
typeof(l)
# ----data structures----
# vector
v <- c(1, 2, 3)
v
is.vector(v)

# matrix
m <- matrix(c(1, 2, 3, 4), nrow = 2)
m

m2 <- matrix(1:6, nrow = 2, byrow = T)
m2

# array
a <- array(c(1:24), c(4, 3, 2))
a

vNum <- c(2, 3, 4)
vChar <- c("a", "B", "c")
vLog <- c(T, F, F)
df <- cbind(vNum, vChar, vLog)
df

df <- as.data.frame(cbind(vNum, vChar, vLog))
df

# list
o1 <- c(1, 2)
o2 <- c("A", "B", "c")
o3 <- c(F)

list1 <- list(o1, o2, o3)
list1

# coercion
c(1, 'b')
# matrix to dataframe
mm <- matrix(1:9, nrow = 3)
is.matrix(mm)

mm <- as.data.frame(mm)
is.data.frame(mm)

# ----factors----
x1 <- 1:3
y <- 1:9

df1 <- cbind.data.frame(x1, y)
typeof(df1$x1)
str(df1)
df1

x2 <- as.factor(c(1:3))
df2 <- cbind.data.frame(x2, y)
str(df2)

x4 <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4, levels = c(1:3), labels = c("mac", "linux", "win"))
df4

# ----entering data----
# colon
(x1 <- 1:10)

(x2 <- 10:2)
# seq
(x1 <- seq(10))

(x2 <- seq(10, 0, by = -3))

# c, for concatenate/combine/collect
(x <- c(5, 1, 2))

# scan
x6 <- scan()
x6
typeof(x6)
is.vector(x6)

# rep
x7 <- rep(T, 5)
(x8 <- rep(c(T, F), each = 2))

# ----importing data----
pacman::p_load(rio)

rio_csv <- import("...")
# same for txt, xlsx
View(rio_csv)
# -----------
p_unload(all)
detach('package:datasets', unload = TRUE)

cat('\014')
