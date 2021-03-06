
# Quiz 1
x<-c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)


mu<-c(0.0025,0.3,1.077,0.1471)

for i in c(1:4) 
{z[i]<-sum(w*(x-mu[i])^2)}

z1<-sum(w*(x-mu[1])^2)
z2<-sum(w*(x-mu[2])^2)
z3<-sum(w*(x-mu[3])^2)
z4<-sum(w*(x-mu[4])^2)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
z<-lm(y~x-1)
summary(z)
z$coeff[1]

library(datasets)
data("mtcars")
lfit<-lm(mtcars$mpg~mtcars$wt,mtcars)
lfit$coeff[2]

x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
x_nor <- (x-mean(x))/sd(x)


x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
z<-lm(y~x)
summary(z)
z$coeff[1]

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

y <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
z<-lm(y~x)
summary(z)