# Linear Regression Models
# Binomial regression
# Exercises 

# e.1
library(datasets)
data(Seatbelts)
sb <- as.data.frame(Seatbelts)
y <- 1*(sb$DriversKilled > 119)
kms <- sb$kms
ms <- kms/1000
mmc <- ms - mean(ms)
pp  <- sb$PetrolPrice
pp <- (pp-mean(pp))/sd(pp)
law <- sb$law
fit <- glm(y~pp+mmc+law, family=binomial)
cf <- summary(fit)$coef
round(cf, 3)
exp(cf[1])
exp(cf[2])
exp(cf[3])
exp(cf[4])

# quiz 4

# q.1
library(MASS)
data(shuttle)
fit <- glm(use ~ wind, data=shuttle, family=binomial)
summary(fit)$coef
exp(summary(fit)$coef[2])

# q.2
fit2 <- glm(use ~ wind + magn, data=shuttle, family=binomial)
summary(fit2)
exp(summary(fit2)$coef[2])
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))

# q.4
data(InsectSprays)
fit <- glm(count~factor(spray), data=InsectSprays, family=poisson)
summary(fit)
exp(summary(fit)$coef[2])

# q.6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])
