# Linear Regression Residuals
# Exercises and quiz 
# Load Galton's dataset
library(UsingR)
library(ggplot2)
data(diamond)
##############################
# Linear regression fitting
# Diamond data example 
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

# Linear model 
p <- diamond$price
ct <- diamond$carat
fit  <- lm(price ~ carat, data=diamond)
coef(fit)

# Centered model (changing mean)
ctc <- ct - mean(ct)
fit2 <- lm(p ~ ctc)
coef(fit2)

# Scaled model (changing the scale)
cts <- ct*10
fit3 <- lm(p ~ cts)
coef(fit3)

# Prediction  - manual
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2]*newx

# Prediction - auto
predict(fit, newdata=data.frame(carat=newx))

##############################
# Exercises

# e.1 fit model
data(father.son)
fh <- father.son$fheight
sh <- father.son$sheight

fit <- lm(sh ~ fh)
summary(fit)

# e.2 recentering data

# e.3 prediction from model
b <- coef(fit)
sh80 <- b[1] + b[2] * 80

# e.4 regression for cars
data(mtcars)
fit <- lm(mpg ~ hp, data=mtcars)
summary(fit)

# e.5 overlay regression on scatterplot
plot(mpg ~ hp, data=mtcars)
abline(fit)

# e.6

# e.7 predict for hp=111
b  <- coef(fit)
mpg111 <- b[1] + b[2] * 111

##############################
# Residuals
# Get residuals from model
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
# automatic residuals
e <- resid(fit)

# manual residuals
yhat <- predict(fit)
e_man <- y-yhat

# Residual variation
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))

# Anscombe's R^2 example
data(anscombe);example(anscombe)


##############################
# Exercises

# e.1 residuals plot
data(father.son)
y <- father.son$sheight
x <- father.son$fheight

# e.1
fit <- lm(y~x)
plot(fit)
plot(x, resid(fit))
abline(v=0, col="red")
# e.2 manual estimation of residual variation
n  <- length(y)
sqrt(sum(resid(fit)^2) / (n - 2))
summary(fit)$sigma

# e.3 calculate R^2
summary(fit)$r.squared

# e.4 fit mpg ~ hp, plot residuals, calc residual var
data(mtcars)
fit <- lm(mpg ~ hp, data=mtcars)
# e.5

# e.6
