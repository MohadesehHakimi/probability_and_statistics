x <- 1:10
x[3]
x[-3]
x[1:3]
x[c(1, 5)]
x[x < 6]

# ----------
# reading data
data <- read.csv()
data <- read.table()

# save data
write.table(Data, file = "filename.csv", sep = ",", row.names = F)
write.csv()
# file.choose()

# inspect data
dim()
head()
tail()
names()

# ---------
library(datasets)
# access using $
mean(iris$Sepal.Length)
sep.len <- iris$Sepal.Length
sep.len

# or attach!
attach(iris)
mean(Sepal.Length)
detach(iris)

names(iris)

class(iris$Sepal.Length)
typeof(iris$Sepal.Length)
summary(iris)

# convert num to factor
x <- c(1, 0, 1, 0, 0)
class(x)
summary(x)
x <- as.factor(x)
class(x)

dim(iris)

sub <- iris[iris$Sepal.Length > 2.5 & iris$Species == "virginica", ]
dim(sub)


# -------
temp <- iris$Sepal.Length > 2.5
temp[1:5]
tmp2 <- as.numeric(iris$Sepal.Length > 2) 
tmp2[1:5]

moredata <- cbind(iris, temp)
moredata


# video 11
getwd()
wd <- "/home/mohadeseh/R scripts"
setwd(wd)
getwd()
# load("workspace.Rdata")

# video 13
install.packages("")
library()
help(package = "")

# video 15
library(datasets)
data(airquality)
summary(airquality)
head(airquality)

apply(X = airquality, MARGIN = 2, FUN = mean)
apply(X = airquality, MARGIN = 2, FUN = mean, na.rm = T)
colMeans(airquality, na.rm = T)
apply(airquality, 2, quantile, probs=c(.20, .80), na.rm = T)
apply(airquality, 2, plot, type = "l", main = "air q")
rowSums(airquality, na.rm = T)

# video 16
x <- c(10, 20, 30, 40, 50)
g <- c("A", "A", "B", "B", "B")

tapply(x, g, mean)
# or
# mean(Age[Smoke=="yes" & Gender=="female"])
tapply(airquality$Ozone, airquality$Month, mean, na.rm = TRUE)
tapply(airquality$Temp,
       list(airquality$Month, airquality$Day),
       mean)

# video 17: bar charts & pie charts
barplot()
pie()

# video 18: boxplot
attach(airquality)
boxplot(Ozone)
boxplot(Ozone ~ Month, main = "title", las = 1)

# video 19
temps <- cut(Temp, breaks = c(56, 72, 79, 77, 85, 97))
temps[1:5]
boxplot(Ozone ~ Month * temps, las = 2, col = c(2, 4))

# video 20
hist(Ozone)
hist(Ozone, freq = F)
hist(Ozone, prob = T)
hist(Ozone, prob = T, breaks = 12)
hist(Ozone, prob = T, breaks = seq(0, 180, by = 10), las = 1)     
lines(density(Ozone))

# video 22
barplot(table, beside = T, legend.text = T)

# video 23
# scatter plot using plot command
plot(Temp, Ozone, las = 1)

# video 24
table(Month)
table(Month, Day)

mean(Ozone, na.rm = T)
median(Ozone, na.rm = T)
var(Ozone, na.rm = T)
sd(Ozone, na.rm = T)
min()
max()
range()
quantile(Ozone, probs=c(0.25, 0.5, 0.75), na.rm = T)
summary(Ozone)
summary(Month)

# video 28
dbinom(x = 3, size = 20, prob = 1/6) # P(x = 3)
dbinom(x = 0:3, size = 20, prob = 1/6) # p(x = 0), p(x = 1)
sum(dbinom(x = 0:3, size = 20, prob = 1/6)) # p(x <= 3)
pbinom(q = 3, size = 20, prob = 1/6) # p(x <= 3), can set lower.tail = F to get x >= 3
# d = density/distribution (exact value), p = probability (<= some value)
# q = quantile (cumulative), r = random

# video 29
dpois(x = 4, lambda = 7)
dpois(x = 0:2, lambda = 7)
sum(dpois(x = 0:4, lambda = 7))
ppois(q = 4, lambda = 7, lower.tail = T)

# p: cdf, d: pdf/pmf, r: random sampling

# video 30
pnorm(q = 70, mean = 75, sd = 5)   # p(x <= 70)
pnorm(q = 1, mean = 0, sd = 1, lower.tail = F)   # p(z >= 1)

x <- seq(55, 95, by = 0.25)
plot(x, dnorm(x, mean = 75, sd = 5))
plot(x, dnorm(x, mean = 75, sd = 5), type = 'l', las = 1)
abline(v = 75)
