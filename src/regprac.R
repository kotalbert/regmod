# Linear Regression Models
# Regression practice  

# load the data
# https://d3c33hcgiwev3.cloudfront.net/_cf0fd3361e05f5be5304b07b771bad48_companydata.csv?Expires=1481673600&Signature=dZMlORGVPxICeRgPtRpKC7sWDuSKNgdK~~rH7jlRubV9PYUgdHdUkzH6XHlPFQTfSoN6uEpnzKgfscn7y61voqeDX2ciqt~z7XUSQTBjHv14wxR5F4o~W1~MJk8q~MXCMsV0k-9x8yp6wdgpKaDFhp~iVs41ShNRQzo6lqXIYMY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A
d <- read.csv("companydata.csv")

# Plot the data
plot(d$x1, d$y)
dev.off()

# Fit the model
fit <- lm(y ~ x1, data=d)
summary(fit)

# Get the ci for the x1 
confint(fit, interval="confidence")
