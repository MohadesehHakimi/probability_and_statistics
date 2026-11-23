library(dplyr)

#### 1 ####
# A
filtered_iris <- iris[iris$Sepal.Length > 5 &
                          iris$Petal.Length > 3.5 &
                          iris$Species == "virginica", ]

# B
rectangularity <- abs(filtered_iris$Sepal.Length - filtered_iris$Sepal.Width)
rectangularity

# C
filtered_iris$Sepal.Rectangularity <- rectangularity

# D
head(filtered_iris)
filtered_iris <- filtered_iris %>% arrange(desc(Sepal.Rectangularity))
head(filtered_iris, 7)

# E
filtered_iris$X <- rowMeans(filtered_iris[, c("Sepal.Rectangularity",
                                              "Petal.Length",
                                              "Petal.Width")]) < 5

# F
mat_iris <- as.matrix(filtered_iris)
mat_iris
colnames(mat_iris)


#### 2 ####
cars <- mtcars
head(cars)

# A
cars$cyl <- sapply(cars$cyl, function(x) x * 2)

# B
aggregate(. ~ disp, data = cars, median)

# C
random_shuffle <- function(df) {
    df[sample(nrow(df)), ]
}

get_top_rows_variance <- function(df, n, col) {
    n <- min(n, nrow(df))
    top_rows <- df[1:n, col]
    var(top_rows)
}

all_vars <- sapply(
    1:100,
    function(i) {
        tmp <- random_shuffle(cars)
        get_top_rows_variance(tmp, i, "disp")
    }
)
all_vars

# D
df <- read.csv('data.csv')
head(df)

sub <- subset(df, Sweet > 2.5)
source <- as.vector(sub$Source)
source

# E
"Pioneer" %in% source

