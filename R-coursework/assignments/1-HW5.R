#### EDA on Palmer Penguins data ####

df <- read.csv('penguins.csv')
attach(df)

head(df)

summary(df)

hist(body_mass_g, breaks = 25)
hist(flipper_length_mm, breaks = 30)
hist(bill_depth_mm, breaks = 30)
hist(bill_length_mm, breaks = 30)

table(sex)
table(island)
table(species)

library(skimr)
skim(df)

detach(df)
df <- na.omit(df)
attach(df)

library(ggplot2)

ggplot(data = df, aes(x = flipper_length_mm, y = body_mass_g)) +
    geom_point()    # strong + correlation

ggplot(data = df, aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_point()
    
cor(flipper_length_mm, body_mass_g, use = 'complete.obs')
cor(bill_depth_mm, bill_length_mm, use = 'complete.obs')

library(GGally)

ggpairs(df)

ggplot(df, aes(bill_length_mm, color = factor(species))) +
    geom_density()

chinstrap <- df[df$species == 'Chinstrap', ]
chinstrap
chinstrap$group <- chinstrap$bill_length_mm > 48

ggplot(chinstrap, aes(x = bill_length_mm, y = bill_depth_mm, 
                      color = factor(chinstrap$group),
                      shape = sex,
                      size = flipper_length_mm,
                      alpha = body_mass_g)) +
    geom_point()
# the two groups generally differ in sex
ggplot(chinstrap, aes(x = bill_length_mm,
                      color = factor(sex))) +
    geom_density()

ggplot(df, aes(x = bill_length_mm, y = bill_depth_mm,
               color = factor(species),
               shape = sex,
               size = flipper_length_mm,
               alpha = body_mass_g)) +
    geom_point()

ggplot(df, aes(x = island,
               fill = species)) + 
    geom_bar(position = 'dodge') +
    theme_minimal()
