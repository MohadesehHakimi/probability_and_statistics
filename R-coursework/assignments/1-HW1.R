library(pacman)
library(dplyr)

pacman::p_load(rio)

# 1- load books.csv
setwd("/home/mohadeseh/Documents/7th semester/Stats & Prob/Assignments/Fall 1401-1402/HW1/HW1_Practical/Question/files")
all_books <- import("books.csv")
# 2- printing the first 5 rows
head(all_books)
tail(all_books)
names(all_books)

# 3- filter the books having less than 10,000 number of ratings
filtered_books <- all_books[all_books$numRatings > 10000, ]

# 4- scale 'rating' column
filtered_books$rating <- filtered_books$rating * 20
summary(filtered_books$rating)

# 5- calculate popularity vector from 'rating' and 'likedPercent'
popularity <- (filtered_books$rating * 5 + filtered_books$likedPercent * 2) / 7

# 6- add popularity vector to the 'filtered-books' dataframe as a column
filtered_books$popularity <- popularity

# 7- sort books based on popularity, publish year and author name in decreasing order
sorted_data <- filtered_books %>%
    arrange(desc(popularity), desc(`publish year`), desc(author))

# 8- write function to take the first n rows of dataframe and get the first 10 best books with it
topn <- function (x, n) {
    return (head(x, n))
}
top_10_books <- topn(sorted_data, 10)

# 9- print 'title', 'author', 'publish year' and 'publisher' of the top_10 books
print(top_10_books[, c('title', 'author', 'publish year', 'publisher')])

# 10- aggregate books based on publish year and count number of books for each year
A <- aggregate(title ~ `publish year`,
               data = filtered_books,
               FUN = length)
A

# 11- aggregate books based on publish year and calculate the maximum number of pages
B <- aggregate(pages ~ `publish year`,
               data = filtered_books,
               FUN = max)
B

# 12- merge tables 'A' and 'B' and print the 15 first rows of resulted table
C <- merge(A, B, by = 'publish year')
topn(C, 15)

