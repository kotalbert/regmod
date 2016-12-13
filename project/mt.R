# Linear Regression Models
# Assignment analysis  

library(ggplot2)
library(dplyr)
library(car)

data(mtcars)
head(mtcars)

ggplot(mtcars, aes(mpg)) + geom_histogram()
dev.off()

# Data seems to be normal
shapiro.test(mtcars$mpg)

# Check the relation 
ggplot(mtcars, aes(factor(am), mpg)) + geom_boxplot()
dev.off()

# Check other variables

# cylinders
ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()
dev.off()

# Displacement
ggplot(mtcars, aes(hp, disp)) + geom_point()
dev.off()

# Horsepower
ggplot(mtcars, aes(hp, mpg)) + geom_point()
dev.off()

# Weight
ggplot(mtcars, aes(wt, mpg)) + geom_point()
dev.off()

# Drat?
ggplot(mtcars, aes(drat, mpg)) + geom_point()
dev.off()

# Qsec
ggplot(mtcars, aes(qsec, mpg)) + geom_point()
dev.off()

# Gear
ggplot(mtcars, aes(factor(gear), mpg)) + geom_boxplot()
dev.off()

# Carb
ggplot(mtcars, aes(factor(carb), mpg)) + geom_boxplot()
dev.off()

# calculate correcations
cor(mtcars)[1,]
fit1 <- lm(mpg ~ factor(am), data=mtcars)
fit2 <- update(fit1, mpg~factor(am)+factor(cyl))
fit3 <- update(fit1, mpg~factor(am)+factor(cyl)+wt)
fit4 <- update(fit1, mpg~factor(am)+factor(cyl)+wt+hp)

anova(fit1, fit2, fit3, fit4)

vif(fit1)
