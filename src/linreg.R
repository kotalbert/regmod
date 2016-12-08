# Linear Regression Models
# Exercises and quiz 
# Load Galton's dataset
library(UsingR)
library(ggplot2)
library(reshape)
library(dplyr)

##########################################
# Data example
# Convert two columns to one with labels
long <- melt(galton)

# Plot two labeled variables
ggplot(long, aes(x=value, fill=variable)) + 
	geom_histogram(colour = "black",  binwidth=1) + 
	facet_grid(. ~ variable)

# Least squares estimate of mu: mean(Y)

# Plot galotn data
ggplot(galton, aes(parent, child)) + geom_point()

# Plot with overlaying size added
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
g

##########################################
# Exercises:

# e.1 least square estimate 
x <- c(0.725,0.429,-0.372 ,0.863)
mean(x)

# e.2 leas square est. weighted data 
w <- c(2,2,1,1)
sum(w*x)/sum(w)

# e.3 centered slope
y <- galton$parent
x <- galton$child
yc <- y - mean(y)
xc <- x - mean(x)
sum(yc * xc)/sum(xc^2)
lm(y ~ x)

##########################################
# Correlation

# e.1 basic stats
x <- galton$parent
y <- galton$child

mx <- mean(x)
sdx <- sd(x)
my <- mean(y)
sdy <- sd(y)
cxy <- cor(x,y)

# e.2 centering
cx <- x - mx
cy <- y - my
mean(cx)
mean(cy)

# e.3 scaling
sx <- x/sdx
sy <- y/sdy
sd(sx)
sd(sy)

# e.4 normalization
zx <- (x-mx)/sdx
zy <- (y-my)/sdy

mean(zx); sd(zx)
mean(zy); sd(zy)

##########################################
# Ordinary least squares

# Calculate coefficients manually 
data(galton)
y <- galton$child
x <- galton$parent

b1 <- cor(y,x) * sd(y)/sd(x)
b0 <- mean(y) - b1*mean(x)
rbind(c(b0, b1), coef(lm(y~x)))

##########################################
# Calculate regression to origin
yc <- y-mean(y)
xc <- x-mean(x)

b1 <- sum(yc*xc)/sum(xc^2)

# correlation of normalized

yn <- (y-mean(y))/sd(y)
xn <- (x-mean(x))/sd(x)

c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

# e.1 linear regression
data(father.son)
y <- father.son$sheight
x <- father.son$fheight
fit <- lm(y~x)

# intercept and slope 
fit$coefficients

# plot the data, add reg line to data
plot(x,y)
abline(fit)

# e.2 center data, fit with no intercept
yc <- y - mean(y)
xc <- x - mean(x)
fit2 <- lm(yc~xc - 1)
c(fit$coefficients[2], fit2$coefficients)

# e.3 normalize data, fit
yn <- yc/sd(y)
xn <- xc/sd(x)
fit3 <- lm(yn~xn)

c(fit3$coefficients[2], cor(yn, xn))

# e.4
y_63 <- fit$coefficients[1] + 63 * fit$coefficients[2]

# Regression to the mean
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g

##########################################
# quiz 1
# q.1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2,1,3,1)
sum(x*w)/sum(w)

# q.2

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
xc <- x - mean(x)
yc <- y - mean(y)

lm(yc~xc - 1)

# q.3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt)

# q.4
# sx = .5sy >> sy/sx = 2
# rho = .5
# b1 = rho * sy/sx = 1

# q.5
1.5*.4

# q.6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x-mean(x))/sd(x)
xn[1]

# q.7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)

# q.9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# quiz 2

# e.1 
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y~x))$coefficients
# e.2 
summary(lm(y~x))$sigma
# e.3 
data(mtcars)
fit.mpg <- lm(mpg ~ wt, data=mtcars)
mwt <- mean(mtcars$wt)
predict(fit.mpg, newdata=list(wt=mwt), interval="confidence")
# e.4 
summary(fit.mpg)
# e.5 
predict(fit.mpg, newdata=list(wt=3), interval="predict")

# e.6 
wtst <- mtcars$wt/2
fit.mpg2 <- lm(mpg ~ wtst, data=mtcars)
summary(fit.mpg2)
confint(fit.mpg2)

# e.9 
y <- mtcars$mpg
yh <- predict(fit.mpg)
sse <- sum((y-yh)^2)
sse2 <- sum((y - mean(y))^2)
sse/sse2
