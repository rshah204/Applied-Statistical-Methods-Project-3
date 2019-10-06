
## Set Working Directory
  dir_path <- getwd()
  setwd(dir_path)
	
## Import Bordeaux Vintage Data 
	dataBV <- read.csv("Proj3_Dataset.csv", header = TRUE)
	str(dataBV)
	summary(dataBV)
	
## Attach dataset
	attach(dataBV)

## Data Description
	## Price = average price relative to 1961, y
	## Temp = temperature(in °C), x1
	## Rain = rain during harvest season (in mm), x2
	## PrevRain = rain pior to growing season (in mm), x3
	## Age = age of vintage, x4

## Boxplot of predictor variables
	par(mfrow = c(1,4))
	boxplot(Temp, main="Temperature (in °C)")
	boxplot(Rain, main="Rain (in mm)")
	boxplot(PrevRain, main="PrevRain (in mm)")
	boxplot(Age, main="Age (in years)")
	
## Plot Relationship between Price and predictor variables
	par(mfrow = c(2,2))
	plot(Price~Temp, xlab = "Temperature (in °C)", ylab = "Price (relative to 1961)")
	plot(Price~Rain, xlab = "Rain (in mm)", ylab = "Price (relative to 1961)")
	plot(Price~PrevRain, xlab = "PrevRain (in mm)", ylab = "Price (relative to 1961)")
	plot(Price~Age, xlab = "Age (in years)", ylab = "Price (relative to 1961)")

## Correlation Matrix of Predictor variables
	dataBV2 <- data.frame(Temp, Rain, PrevRain, Age)
	cor(dataBV2)
	
## Scatter plot of predictor variables
	plot(dataBV2)
	
### Inverse transform
	PriceInv=1-1/Price
	TempInv=1-1/Temp
	RainInv=1-1/Rain
	PrevRainInv=1-1/PrevRain
	AgeInv=1-1/Age
	
	par(mfrow = c(2,2))
	plot(PriceInv~TempInv, xlab = "Temperature (in °C)", ylab = "Price (relative to 1961)")
	plot(PriceInv~RainInv, xlab = "Rain (in mm)", ylab = "Price (relative to 1961)")
	plot(PriceInv~PrevRainInv, xlab = "PrevRain (in mm)", ylab = "Price (relative to 1961)")
	plot(PriceInv~AgeInv, xlab = "Age (in years)", ylab = "Price (relative to 1961)")
	
	
	logTemp <- log10(Temp)

	plot(logPrice~logTemp, xlab = "Log(Temperature)", ylab = "Log(Price)")

# ## Step 2: Plot Relationship between logPrice and x6
# 	plot(logPrice~x6, xlab = "Batch", ylab = "Log(Price)")

## Log transformation of response variable
	logPrice <- log(Price)
	
## Plot Relationship between LogPrice and predictor variables
	par(mfrow = c(2,2))
	plot(logPrice~Temp, xlab = "Temperature (in °C)", ylab = "Log(Price)")
	plot(logPrice~Rain, xlab = "Rain (in mm)", ylab = "Log(Price)")
	plot(logPrice~PrevRain, xlab = "PrevRain (in mm)", ylab = "Log(Price)")
	plot(logPrice~Age, xlab = "Age (in years)", ylab = "Log(Price)")

## Plotting square relationships
	Temp2 = Temp^2
	Rain2 = Rain^2
	PrevRain2 = PrevRain^2
	Age2 = Age^2
	
	plot(logPrice~Temp2, xlab = "Temperature2 (in °C)", ylab = "Log(Price)")
	plot(logPrice~Rain2, xlab = "Rain2 (in mm)", ylab = "Log(Price)")
	plot(logPrice~PrevRain2, xlab = "PrevRain2 (in mm)", ylab = "Log(Price)")
	plot(logPrice~Age2, xlab = "Age2 (in years)", ylab = "Log(Price)")

## Run Regression Model
	lm1 <- lm(Price~Temp + Rain + PrevRain + Age)
	summary(lm1) 

## Residual Plots
	par(mfrow = c(1,2))
	plot(lm1, which = 1:2)
	
## Residual Plots e_i vs x_i
	par(mfrow = c(2,2))
	plot(resid(lm1)~Temp)
	abline(h = 0)
	plot(resid(lm1)~Rain)
	abline(h = 0)
	plot(resid(lm1)~PrevRain)
	abline(h = 0)
	plot(resid(lm1)~Age)
	abline(h = 0)
	
## Find r1: Lag 1 Autocorrelation
	acf(resid(lm1), lag.max = 1, plot = FALSE)
	n = 27
	2 / sqrt(n)
	
## Durbin-Watson Statistic
## H_0: true autocorrelation is 0
## H_1: true autocorrelation is not 0
	library(lmtest)
	dwtest(lm1, alternative = "two.sided")
	
## Run Regression Model on logPrice
	lm2 <- lm(logPrice~Temp + Rain + PrevRain + Age)
	summary(lm2) 
	
## Residual Plots
	par(mfrow = c(1,2))
	plot(lm2, which = 1:2)
	
## Residual Plots e_i vs x_i
	par(mfrow = c(2,2))
	plot(resid(lm2)~Temp)
	abline(h = 0)
	plot(resid(lm2)~Rain)
	abline(h = 0)
	plot(resid(lm2)~PrevRain)
	abline(h = 0)
	plot(resid(lm2)~Age)
	abline(h = 0)
	
## Find r1: Lag 1 Autocorrelation
acf(resid(lm2), lag.max = 1, plot = FALSE)

## Quadratic terms
	Temp2 = Temp^2
	Rain2 = Rain^2
	PrevRain2 = PrevRain^2
	Age2 = Age^2
	
## Run Regression Model on logPrice with quadratic 
## predictors added one at a time
	lm3 <- lm(logPrice~Temp + Rain + PrevRain + Age + Temp2)
	lm4 <- lm(logPrice~Temp + Rain + PrevRain + Age + Rain2)
	lm5 <- lm(logPrice~Temp + Rain + PrevRain + Age + PrevRain2)
	lm6 <- lm(logPrice~Temp + Rain + PrevRain + Age + Age2)
	
	summary(lm3) 
	
	sum(resid(lm3)^2)
	
## Residual Plots
	par(mfrow = c(1,2))
	plot(lm3, which = 1:2)
	
	shapiro.test(resid(lm3))
	
## Residual Plots e_i vs x_i
	par(mfrow = c(2,3))
	plot(resid(lm3)~Temp)
	abline(h = 0)
	plot(resid(lm3)~Rain)
	abline(h = 0)
	plot(resid(lm3)~PrevRain)
	abline(h = 0)
	plot(resid(lm3)~Age)
	abline(h = 0)
	plot(resid(lm3)~Temp2)
	abline(h = 0)
	
## Find r1: Lag 1 Autocorrelation
  acf(resid(lm3), lag.max = 1, plot = FALSE)
	
## Gives partial t-test values and Regression F Value
	
## Step 4: Run Regression Model
	lm.dataBV <- lm(logPrice~Temp + Rain + PrevRain + Age) ## + signs means to add variables to model without interactions
	lm.dataBV <- lm(logPrice~Temp + Rain + PrevRain + Age + Temp2) ## R2=86% 
	lm.dataBV <- lm(Price~Temp + Rain + PrevRain + Age)
	lm.dataBV <- lm(PriceInv~Temp + Rain + PrevRain + Age)
	lm.dataBV <- lm(Price~Temp*Rain*PrevRain*Age)
	
	PriceInvSqrt <- 1/sqrt(Price)
	lm.dataBV <- lm(PriceInvSqrt~Temp + Rain + PrevRain + Age)
	
	PriceInvCubrt <- Price^(-0.3)
	lm.dataBV <- lm(PriceInvCubrt~Temp + Rain + PrevRain + Age)
	
	summary(lm.dataBV) ## Gives partial t-test values and Regression F Value
	a.dataBV <- anova(lm3) ## Gives ANOVA Table
	a.dataBV
	a.dataBV <- anova(lm.dataBV, lm.dataBV2) ## partial f-test: anova(reduced, full model). if H0 rejected, full model is significant

	## BoxCox Plot
	library("MASS")
	bc = boxcox(Price ~ Temp + Rain + PrevRain + Age, data=dataBV)
	lambda = bc$x[which.max(bc$y)] ## Optimal lambda
	
	## Combine DF and SS for Regression Source
	regr <- a.dataBV[1,c(1,2)] + a.dataBV[2,c(1,2)] +
		a.dataBV[3,c(1,2)] + a.dataBV[4,c(1,2)] 
	regr
	
	## Mean Square Regression Value
	msr <- regr[,2] / regr[,1]

	## Mean Square Error Value
	mse <- a.dataBV[5,3]

	## F Value (MSR / MSE)
	f <- msr / mse		
	f

	## Probability > F
	pf(f, regr[,1], a.dataBV[5,1], lower.tail = FALSE)

	## Critical Value for F if alpha = 0.01
	qf(0.01, regr[,1], a.dataBV[5,1], lower.tail = FALSE)

	## Residual Plots
	par(mfrow = c(1,2))
	plot(lm.dataBV, which = 1:2)

	## Residual Plots e_i vs x_i
	par(mfrow = c(2,3))
	plot(resid(lm.dataBV)~logTemp)
	abline(h = 0)
	plot(resid(lm.dataBV)~Rain)
	abline(h = 0)
	plot(resid(lm.dataBV)~PrevRain)
	abline(h = 0)
	plot(resid(lm.dataBV)~Age)
	abline(h = 0)
	
	## Find r1: Lag 1 Autocorrelation
	## Use residual from our original linear model,
	## where we have enrol~time
	e = residuals(lm.dataBV)
	acf(e, lag.max = 1, plot = FALSE)
	n = 27
	2 / sqrt(n)
	
	## if abs(r1) > 2 / sqrt(n),
	## then independence of errors
	## is not necessarily reasonable
	
	## since 0.634 > 0.485,
	## independence of errors is not reasonable
	
	
	## Durbin-Watson Statistic
	## H_0: true autocorrelation is 0
	## H_1: true autocorrelation is not 0
	library(lmtest)
	dwtest(enrol~time, alternative = "two.sided")
	
	## since pval = 3.95e-07, reject H_0
	## lag 1 autocorrelation present among the errors
	## independence of errors is not reasonable
	
	

## Step 5: Remove Age by hand
	lm.dataBV2 <- lm(logPrice~logTemp + Rain + PrevRain)
	anova(lm.dataBV2)
	summary(lm.dataBV2)

	## Residual Plots
	par(mfrow = c(2,2))
	plot(lm.dataBV)

	## Residual Plots e_i vs x_i
	par(mfrow = c(2,2))
	plot(resid(lm.dataBV)~logTemp)
	abline(h = 0)
	plot(resid(lm.dataBV)~Rain)
	abline(h = 0)
	plot(resid(lm.dataBV)~PrevRain)
	abline(h = 0)

## Step 5: Remove PrevRain by hand
	lm.dataBV3 <- lm(logPrice~logTemp + Rain)
	anova(lm.dataBV3)
	summary(lm.dataBV3)

	## Residual Plots
	par(mfrow = c(2,2))
	plot(lm.dataBV)

	## Residual Plots e_i vs x_i
	par(mfrow = c(2,2))
	plot(resid(lm.dataBV)~logTemp)
	abline(h = 0)
	plot(resid(lm.dataBV)~Rain)
	abline(h = 0)

	
## Backward Elimination
	dataBV2 <- data.frame(logPrice, Temp, Rain, PrevRain, Age, Temp2)
	full = lm(logPrice ~ ., data = dataBV2) ## Saves the full model 
	step(full, data = dataBV2, direction = "backward")

## Forward Selection
	null = lm(logPrice ~ 1, data = dataBV2) ## ~1 = intercept only
	step(null, scope = list(lower = null, upper = full),
		direction = "forward")


