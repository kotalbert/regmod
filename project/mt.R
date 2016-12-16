# Linear Regression Models
# Assignment analysis  

library(ggplot2)
library(dplyr)
library(car)
library(GGally)

data(mtcars)
head(mtcars)

# The target variable - check for normality
ggplot(mtcars, aes(mpg)) + geom_histogram(fill="lightblue", color="black")
shapiro.test(mtcars$mpg)

# check correlated variables
cor(mtcars)[1,]

# Transform cyl and am to factors
# Select only highly correlated factors
mtc2 <- mtcars %>% 
	dplyr::mutate(cylf = factor(cyl), amf=factor(am)) %>%
	dplyr::select(mpg, cylf, amf,  disp, hp, wt)

# pairs plot for selected variables
ggpairs(mtc2)

# Model selection procedure
fit1 <- lm(mpg ~ amf, data=mtc2)
fit2 <- update(fit1, mpg~amf+cylf)
fit3 <- update(fit1, mpg~amf+cylf+disp)
fit4 <- update(fit1, mpg~amf+cylf+disp+wt)
fit5 <- update(fit1, mpg~amf+cylf+disp+wt+hp)
anova(fit1, fit2, fit3, fit4, fit5)
summary(fit5)
fit6 <- update(fit1, mpg~amf+wt+hp)
anova(fit1, fit2, fit3, fit4, fit5, fit6)
summary(fit6)
vif(fit5)
vif(fit6)

# Residuals diagnostics
plot(fit5, which=1)
res <- fit5$residuals
hist(fit5$residuals)
shapiro.test(fit5$residuals)
