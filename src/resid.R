# Linear Regression Residuals
# Residuals 
# Excercises & quiz 3

# e.1 fit model
library(datasets)
data(Seatbelts)
sb <- as.data.frame(Seatbelts)
fit  <- lm(sb$DriversKilled ~ sb$kms + sb$PetrolPrice + sb$law)
summary(fit)

# e.2 residual variation
sum(resid(fit)^2)/(nrow(sb)-4)
summary(fit)$sigma^2

# e.3 Analysis of diagnostic measures
# plot diagnostic measures
plot(fit)
dev.off()

# Residual influence plot
plot(dffits(fit))
dev.off()

# Residuals leverage plots for coefficients
plot(dfbetas(fit)[,2])
dev.off()

plot(dfbetas(fit)[,3])
dev.off()

# quiz 3
# q.1
data(mtcars)
fit <- lm(mpg~factor(cyl)+wt, mtcars)
summary(fit)

# q.2
fit <- lm(mpg~factor(cyl)+wt, mtcars)
fit2 <- lm(mpg~factor(cyl), mtcars)
summary(fit)
summary(fit2)

# q.3
fit <- lm(mpg~factor(cyl)+wt, mtcars)
fit3 <- lm(mpg~factor(cyl)*wt, mtcars)
anova(fit, fit3)

# q.4
fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit)
summary(fit4)

# q.5 q.6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y~x)

hatvalues(fit)
dfbetas(fit)
