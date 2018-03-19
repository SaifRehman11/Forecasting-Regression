library(forecast)
library(tseries)

GasData <- read.csv("GasData.csv")

#plot the variable called "unleaded" 
plot(GasData$Unleaded, type = "l")

#Scatterplot Matrix
plot(GasData)
cor(GasData)

plot(GasData$Unleaded, GasData$L1_Crude, col = "blue")

#Fit a regression
lm1 <- lm(Unleaded ~ L1_Crude + L1_SP500 + L1_RR_Sales + L1_Unemp, data = GasData)

#Fit the same regression
lm(Unleaded ~ ., data = GasData)

#Summary
summary(lm1)


#Regression coefficients
coef(summary(lm1))


#Fit a regression without intercept
lm(Unleaded ~ L1_Crude + L1_Unemp + L1_SP500 + L1_RR_Sales - 1, data = GasData)

#Fit a regression with a transformed variable
lm(Unleaded ~ L1_Crude + L1_Unemp + I(log(L1_SP500)) + L1_RR_Sales - 1, data = GasData)

#Extract R Squared
summary(lm1)$r.squared

#Extract adjusted R-squared
summary(lm1)$adj.r.squared

#Extract standard error
sigma(lm1)


#Extract residuals 
lm1_resid <- residuals(lm1)

#Extract Residuals
lm1_fitted <- fitted(lm1)


#Plot Histogram
hist(lm1_resid)

#QQ Plot
qqnorm(lm1_resid)
qqline(lm1_resid)

#Jarque-bera test
jarque.bera.test(lm1_resid)

#Shapiro-Wilk test 
shapiro.test(lm1_resid)

#Kolmogorov-Smirnov test
ks.test(lm1_resid, y = "rnorm")

#Plot Residuals against Fitted Values
plot(lm1_fitted, lm1_resid)

#Plot Squared Residuals against Fitted Values
plot(lm1_fitted, lm1_resid^2)

#Plot Residuals against all x variables
pairs(lm1_resid ~ . , data = GasData[,-1])

#Plot Residuals against Time
plot(lm1_resid, type = "l")


#Plot Residuals against Time
plot(lm1_resid, type = "l")
#Draw the mean line at 0 in Blue
abline(h=0, col = "blue")


#ACF and the PACF of the residuals 
tsdisplay(lm1_resid)


#Create studentized residuals 
lm1_resid_st <- rstandard(lm1)

#Plot the Residuals
plot(lm1_resid_st)
#Draw two horizontal lines at 2 and -2 
abline(h = c(-2,2), col = "red")


