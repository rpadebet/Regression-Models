
comp_dat <- read.csv("company_data.csv",header = TRUE,stringsAsFactors = FALSE)
plot(comp_dat$x1,comp_dat$y)
modl<-lm(y~x1,comp_dat)
summary((modl))
# Slope
round(summary(modl)$coef[2],6)
#Confidence Interval
confint(modl,"x1",0.95)
# P Value
summary(modl)$coef[2,4]


modl3<-lm(y~.,comp_dat)
summary(modl3)

plot(comp_dat$x1,comp_dat$y)
