library(ggplot2)

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy,
                             color = class))

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy),
                             color = 'red')

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy,
                             color = drv)) +
    geom_smooth(mapping = aes(x = displ, y = hwy,
                              color = drv,
                              linetype = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy,
                                 color = drv)) +
    geom_point() +
    geom_smooth(aes(linetype = drv))

df <- read.csv('Salaries.csv')
head(df)

ggplot(data = df, mapping = aes(x = yrs.service, y = salary,
                                color = salary > 100000)) + 
    geom_point()

# what's going on here?
ggplot(data = df, mapping = aes(x = yrs.service, y = salary,
                                color = 'green')) + 
    geom_point()
# color is defined inside aes. ggplot has defined a new variable and assigned it
# to all datapoints.

ggplot(data = df, mapping = aes(x = yrs.service, y = salary,
                                color = rank,
                                shape = sex)) + 
    geom_point()

ggplot(data = df) +
    geom_histogram(aes(x = salary), bins = 25)

ggplot(df, aes(x = sex, y = salary, fill = sex)) +
    geom_boxplot()

ggplot(df) +
    geom_density(aes(x = salary, fill = rank, alpha = 0.5))

ggplot(df) +
    geom_point(aes(x = yrs.service, y = salary, color=yrs.since.phd))

ggplot(df) +
    geom_point(aes(x = yrs.service, y = salary, size=yrs.since.phd))

ggplot(df, aes(x = rank, y = salary)) +
    geom_violin() +
    geom_boxplot(width = 0.1)



?diamonds
head(diamonds)

ggplot(diamonds) +
    geom_bar(aes(x = cut))

ggplot(diamonds) +
    stat_count(aes(x = cut))

df <- data.frame(cut = c('fair', 'good', 'very good', 'premium', 'ideal'),
                 count = c(100, 120, 200, 300, 400))
ggplot(df) +
    geom_bar(aes(x = cut, y = count))   # error!

ggplot(df) +
    geom_bar(aes(x = cut, y = count), stat = 'identity')

# geom_col: geom_bar yet stat = 'identity'

