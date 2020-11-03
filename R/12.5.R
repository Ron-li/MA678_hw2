#12.5a
exp(0.25)
-exp(0.25)
#exp(0.25)=1.28and-exp(0.25)=-1.28

#12.5(b)
set.seed(1)
female_height <- data.frame(rnorm(100, 61.4, 3))
male_height <- data.frame(rnorm(100, 65.7, 3.1))
colnames(female_height) <- "height"
colnames(male_height) <- "height"
total_height <- rbind(female_height, male_height)
weight <- exp(-3.8 + 2.1 * log(total_height$height) + rnorm(200, 0, 0.25))
df <- data.frame(weight, total_height)
df$logweight <- log(df$weight)
df$logheight <- log(df$height)
fit <- lm(logweight~logheight, data = df)
plot(df$logheight, df$logweight, xlab = "log(height)", ylab = "log(weight)", main = "Datas and the regression line")
abline(fit)
