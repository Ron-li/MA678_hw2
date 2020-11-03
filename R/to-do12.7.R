library("foreign")
library("rstanarm")
library("loo")
library("ggplot2")
library("bayesplot")

earning <- read.csv("/Users/amelia/Documents/mssp/MA678/hw2/earnings.csv", header = T)
earning$log_earnk <- log(earning$earnk)
df <- earning[,c(1,3,5,16)]

#12.7(a)
#earnk~height+male
fit1 <- stan_glm(earnk ~ height + male, data = df, subset = earnk > 0, refresh=0)
print(fit1)
kfold_1 <- kfold(fit1, K=10)

fit2 <- stan_glm(log(earnk) ~ height + male, data = earning, subset = earnk > 0, refresh = 0)
print(fit2)

(loo_1 <- loo(fit1))
(loo_2 <- loo(fit2))

loo_2_with_jacobian <- loo_2
loo_2_with_jacobian$pointwise[,1] <- loo_2_with_jacobian$pointwise[,1] - log(df$earnk[df$earnk>0])
(elpd_loo_2_with_jacobian <- sum(loo_2_with_jacobian$pointwise[,1]))

loo_compare(kfold_1, loo_2_with_jacobian)

#12.7(b)











