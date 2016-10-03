

library(datasets)
data("mtcars")
summary(mtcars)
names(mtcars)
head(mtcars)

lfit<-lm(mpg~wt+factor(cyl),mtcars)

summary(lfit)$coeff

lfit2<-lm(mpg~factor(cyl),mtcars)

summary(lfit2)$coeff

lfit3<-lm(mpg~wt*factor(cyl),mtcars)

summary(lfit3)

anova(lfit,lfit3)

summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))$coeff


x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fitl<-lm(y~x)
summary(fitl)

hatvalues((fitl))
dfbetas(fitl)

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

# trying this out


