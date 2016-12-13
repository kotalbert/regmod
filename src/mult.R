# Linear Regression 
# Multivariable regression analysis
# Exercises
library(UsingR)
library(ggplot2)

##############################

# Simulation demonstarion
n <- 100
x <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- 1 + x + x2 + x3 + rnorm(n, sd=.1)

ey <- resid(lm(y~x2 + x3))
ex <- resid(lm(x~x2 + x3))

sum(ey*ex)/sum(ex^2)
coef(lm(ey~ex-1))
coef(lm(y~x+x2+x3))

# e.1
data(Seatbelts)
sb <- as.data.frame(Seatbelts)
fit  <- lm(DriversKilled ~ kms + PetrolPrice, sb)
summary(fit)

# e.2
akm <- mean(sb$kms)
app <- mean(sb$PetrolPrice)

predict(fit, newx=data.frame(kms=akm, PetrolPrice=app))

# e.3
dk <- sb$DriversKilled
kms <- sb$kms
pp <- sb$PetrolPrice

fitfull <- lm(dk~kms+pp)
edk <- resid(lm(dk~kms))
epp <- resid(lm(pp~kms))
summary(lm(edk~epp -1))$coef
summary(fitfull)$coef

# e.4
edk <- resid(lm(dk~pp))
ekm <- resid(lm(kms~pp))
summary(lm(edk~ekm-1))$coef

##############################

library(datasets) 
data(swiss)

summary(lm(Fertility ~ ., data=swiss))

# Simpson's Paradox - reversed slope of factor
summary(lm(Fertility ~ Agriculture, data=swiss))

# Simulation example

n <- 100
x2 <- 1:n
x1 <- .01*x2+runif(n,-.1,.1)
y <- -x1+x2+rnorm(n,sd=.01)
summary(lm(y~x1))$coef
summary(lm(y~x1+x2))$coef

par(mfrow=c(1,2))
plot(x1,y)
plot(resid(lm(x1~x2)), resid(lm(y~x2)))

##############################

# Dummy variables

library(datasets)
library(ggplot2)
data(InsectSprays)

g <- ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g <- g + geom_violin(colour = "black", size = 2)
g <- g + xlab("Type of spray") + ylab("Insect count")
g

# Intercept: mean for reference factor and change in regard to
# reference factor
summary(lm(count ~ spray, InsectSprays))$coef

# No intercept, estimates of mean in each group
summary(lm(count ~ spray - 1, InsectSprays))$coef

# Interactions of variables
swiss$CatholicBin <- 1*(swiss$Catholic > 50)

g <- ggplot(swiss, aes(x=Agriculture, y=Fertility, colour=factor(CatholicBin))) +
geom_point(size=4, colour="black") + geom_point(size=2) + 
xlab("% in Agriculture") + ylab("Fertility")

# 1 intercept, 1 slope
summary(lm(Fertility~Agriculture, swiss))$coef

# 2 interceptsc, 1 splope
summary(lm(Fertility~Agriculture+factor(CatholicBin), swiss))$coef

# 2 intercepts, 2 slopes
summary(lm(Fertility~Agriculture*factor(CatholicBin), swiss))$coef

# Excercises

# e.1 fit linear model
data(Seatbelts)
sb <- as.data.frame(Seatbelts)
dk <- sb$DriversKilled
kms <- sb$kms
pp <- sb$PetrolPrice
fit <- lm(dk ~ kms + pp)
summary(fit)

# e.2 fit log of counts
ldk <- log(dk)
fit2 <- lm(ldk~kms+pp)
summary(fit2)

# e.3

# e.4

# e.5
