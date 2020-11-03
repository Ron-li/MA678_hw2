library(rstanarm)

pyth <- read.table("/Users/amelia/Documents/mssp/MA678/hw2/pyth.txt", header = T)

#11.5(a)
data1 <- head(pyth, 40)
fit1 <- lm(y~x1+x2, data = data1)
print(fit1)
summary(fit1)["r.squared"]
#y = 1.3 + 0.5x1 + 0.8x2
#The r2 is close to 1, so the fit is good. 

#11.5(b)
par(mfrow=c(1,2))
beta <- coef(fit1)
x1_bar <- mean(data1$x1)
x2_bar <- mean(data1$x2)
figure1 <- plot(data1$x1, data1$y, xlab = "x1", ylab = "y")
abline(beta[1] + beta[3]*x2_bar, beta[2], col = "blue")
figure2 <- plot(data1$x2, data1$y, xlab = "x2", ylab = "y")
abline(beta[1] + beta[2]*x1_bar, beta[3], col = "blue")

#11.5(c)
par(mfrow=c(1,1))
predicted <- predict(fit1)
residual <- data1$y - predicted
sigma <- sigma(fit1)
df <- data.frame(data1, predicted, residual)
plot(df$predicted, df$residual, xlab = "predicted y", ylab = "prediction error")
abline(c(0, 0), c(0, sigma), lwd=.5)
#The residuals are centered around zero for all fitted values.But the distribution is not normal.

par(mfrow=c(1,3))
plot(df$predicted, df$residual, xlab = "predicted y", ylab = "prediction error")
plot(df$x1, df$residual, xlab = "x1", ylab = "prediction error")
plot(df$x2, df$residual, xlab = "x2", ylab = "prediction error")

#11.5(d)
data2 <- pyth[41:60,]
predict (fit1, data2, interval="prediction", level=0.95)
#The residual plot may indicate issues (specifically the linearity assumption) so not quite sure of the goodness of the predictions.
