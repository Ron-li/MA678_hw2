library(rstanarm)
library(ggplot2)

pollution <- read.csv("/Users/amelia/Documents/mssp/MA678/hw2/pollution.csv", header = T)

#12.6(a)
plot(pollution$nox, pollution$mort, xlab = "level of nitric oxides", ylab = "mortality rate")
fit1 <- lm(mort~nox, data = pollution)
print(fit1)
summary(fit1)["r.squared"]
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))
plot(pollution$nox, pollution$mort)
abline(fit1)
#I think linear regression will not fit the data well.

#12.6(b)
fit2 <- lm(log(mort)~log(nox), data = pollution)
print(fit2)
summary(fit2)["r.squared"]
ggplot(data = pollution, aes(x = log(nox), y = log(mort))) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y~x)
par(mfrow=c(2,2))
plot(fit2)

#12.6(c)
exp(6.18)
#The intercept. exp(6.81) is the expected mortality rate of areas with nitric oxides = 1.
#The coefficient of log(nox).For each 1% difference in height, the predicted difference in mortality rate is 0.02%

#12.6(d)


