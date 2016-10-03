library(MASS)

?shuttle
View(shuttle)
summary(shuttle)

shuttle$wind <- relevel(shuttle$wind,"head")
shuttle$use <- relevel(shuttle$use,"noauto")

mdl<-glm(use ~ wind-1,"binomial",shuttle)
summary(mdl)

exp(mdl$coefficients[1]-mdl$coefficients[2])

mdl2<-glm(use ~ wind+magn,"binomial",shuttle)
summary(mdl2)
exp(mdl$coefficients[1]-mdl$coefficients[2])

shuttle$use <- relevel(shuttle$use,"auto")
mdl3<-glm(use ~ wind-1,"binomial",shuttle)
summary(mdl3)
exp(mdl$coefficients[1]-mdl$coefficients[2])

str(InsectSprays)
InsectSprays$spray<-relevel(InsectSprays$spray,"B")

mdl4<-glm(count ~ spray-1, "poisson",InsectSprays)
summary(mdl4)



x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knot <- ifelse(x>0,x,0)

xmat<-cbind(1,x,knot)
xmat

mdl5<- lm(y~xmat)
yhat<-predict(mdl5)
plot(x,y,pch=21,bg="lightblue")
lines(x,yhat,col="red",lwd=2)

summary(mdl5)
