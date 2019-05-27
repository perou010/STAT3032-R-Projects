library(alr4)
attach(wblake)
wblakeRegression <- lm(Length~Age)
z <- predict(wblakeRegression, data.frame(Age=2), se.fit = TRUE)
CI.se <- z$se.fit

#PI.se <- sqrt(deviance(wblakeRegression)/df.residual(wblakeRegression)) * sqrt(1 + 1/df.residual(wblakeRegression) + (2 - mean(Age))^2/(sum((Age - mean(Age))^2)))
PI.se <- sqrt(z$se.fit ^ 2+ z$residual.scale ^ 2) 

alpha <- qt((1-.95)/2, df=z$df)

CI <- z$fit + c(alpha, -alpha) * CI.se
PI <- z$fit + c(alpha, -alpha) * PI.se

plot(Age, Length, pch ='.')
abline(CI[1], 0)
abline(CI[2], 0)
abline(PI[1], 0, lty = 2)
abline(PI[2], 0, lty = 2)
abline(wblakeRegression)
