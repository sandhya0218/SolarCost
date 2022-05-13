# Exploratory Data Analysis

## Scatterplots
plot(SolarCost$`Roof sqft Exposed`, SolarCost$`Solar Cost`, xlab = "`Roof SqFt Exposed`", ylab = "`Solar Cost`", main = "`Roof SqFt Exposed` vs. `Solar Cost`")
plot(SolarCost$`Yr. Hours Sunlight`, SolarCost$`Solar Cost`, xlab = "`Yr. Hours Sunlight`", ylab = "`Solar Cost`", main = "`Yr. Hours Sunlight vs. `Solar Cost`")
plot(SolarCost$`Kwh potential`, SolarCost$`Solar Cost`, xlab = "`Kwh potential`", ylab = "`Solar Cost`", main = "`Kwh potential` vs. `Solar Cost`")
plot(SolarCost$year_built, SolarCost$`Solar Cost`, xlab = "Year Built", ylab = "`Solar Cost`", main = "Year Built vs. Solar Cost" )

## Scatterplot in ggplot2
library(ggplot2)
ggplot(SolarCost) + geom_point(aes(x = `Roof sqft Exposed`, y = `Solar Cost`, color  = `Property Group`)) + labs(title = "Roof SqFt. Exposed vs. Solar Cost", x = "Roof SqFt. Exposed")

## Histograms
hist(SolarCost$`Solar Cost`, xlab = "Solar Cost", main = "Distribution of `Solar Cost`")
hist(SolarCost$`Roof sqft Exposed`, xlab = "Square Feet of Roof Exposed", main = "Distribution of `Roof SqFt Exposed`")
hist(SolarCost$`Kwh potential`, xlab = "KwH Potential", main = "Distribution of `KwH Potential`")
hist(SolarCost$year_built, xlab = "Year Built", main = "Distribution of `Year Built`")
hist(SolarCost$`Yr. Hours Sunlight`, xlab = "`Yr. Hours Sunlight`", main = "Distribution of `Yr. Hours Sunlight`")

# Boxplot
ggplot(SolarCost) + geom_boxplot(aes(x = `Property Group`, y = `Solar Cost`)) + labs(title = "Property Group vs. Solar Cost Boxplot")

# Numeric Summaries

## Mean
mean(SolarCost$year_built)
mean(SolarCost$`Solar Cost`)
mean(SolarCost$`Kwh potential`)
mean(SolarCost$`Roof sqft Exposed`)

## Standard Deviation
sd(SolarCost$year_built)
sd(SolarCost$`Solar Cost`)
sd(SolarCost$`Kwh potential`)
sd(SolarCost$`Roof sqft Exposed`)
sd(SolarCost$`Yr. Hours Sunlight`)

# 5-Number Summary
summary(SolarCost$year_built)
summary(SolarCost$`Solar Cost`)
summary(SolarCost$`Kwh potential`)
summary(SolarCost$`Roof sqft Exposed`)
summary(SolarCost$`Yr. Hours Sunlight`)


# Statistical Models used to attempt to fix the violations seen in the model validity check for the first business question. 
# These five models are combinations of log transformations of the response and the predictors, both independently and together.


# Model 0: Original Model
fit0 <- lm(log(`Solar Cost`)~log(`Roof sqft Exposed`), data = SolarCost)
qqnorm(residuals(fit0)); qqline(residuals(fit0)) # heavy tails
plot(predict(fit0), residuals(fit0)) # even spread of residuals

# Model 1: transformation of only the response in MLR
fit_1 <- lm(log(`Solar Cost`)~`Roof sqft Exposed`, data = SolarCost)
qqnorm(residuals(fit_1)); qqline(residuals(fit_1)) # slightly improved the tails from original model; tails are at right angles
plot(predict(fit_1), residuals(fit_1)) # quadratic pattern of residuals

# Model 2: transformation of only predictors in MLR
fit_2 <- lm(`Solar Cost`~log(`Roof sqft Exposed`), data = SolarCost)
qqnorm(residuals(fit_2)); qqline(residuals(fit_2)) # slightly improved the tails from original model; tails are at right angles
plot(predict(fit_2), residuals(fit_2)) # quadratic pattern of residuals

# Model 3: transformation of response and predictors in MLR
fit_3 <- lm(log(`Solar Cost`)~log(`Roof sqft Exposed`)+log(`Yr. Hours Sunlight`), data = SolarCost)
qqnorm(residuals(fit_3)); qqline(residuals(fit_3)) # fixes normality assumption
plot(predict(fit_3), residuals(fit_3)) # creates multiplicative relationship of residuals



# Business Question #1: Bootstrapping before standardized predictors (PDF Source)

library(car); library(boot)

fit <- lm(`Solar Cost`~`Roof sqft Exposed`+`Yr. Hours Sunlight`, data = SolarCost)
summary(fit)

# Plots used for checking Model Utility
plot(predict(fit), residuals(fit))
qqnorm(residuals(fit)); qqline(residuals(fit))
CV <- 100*(11.68/(mean(SolarCost$`Solar Cost`)))

# Bootstrapping
library(boot)
set.seed(12345) # for reproducability
system.time(solar.boot <- Boot(fit, R=1999))

summary(solar.boot, high.moments=TRUE)
vcov(solar.boot)

hist(solar.boot, legend="separate")

# Standardizing Predictors for the first business question
fitST <- lm(`Solar Cost` ~ scale(`Roof sqft Exposed`) + scale(`Yr. Hours Sunlight`), data = SolarCost)
summary(fitST)

set.seed(12345) # for reproducability
system.time(solar.boot <- Boot(fit, R=1999))

summary(solar.boot, high.moments=TRUE)

# Bootstrap Confidence Intervals
confint(solar.boot, type = "norm")

# Depicts uniform distribution of residuals
hist(residuals(fit2))



# Business Question 2: Effect of Property Group
fit2 <- lm(`Solar Cost` ~ `Roof sqft Exposed`+`Yr. Hours Sunlight` + `Property Group`, data = SolarCost)
summary(fit2)

# Plots used for checking Model Utility
plot(predict(fit2), residuals(fit2))
qqnorm(residuals(fit2)); qqline(residuals(fit2))
CV2 <- 100*(11.68/(mean(Solar_Cost$`Solar Cost`)))

# Bootstrapping
library(boot)
set.seed(12345) # for reproducability
system.time(solar.boot <- Boot(fit2, R=1999))

summary(solar.boot, high.moments=TRUE)
vcov(solar.boot)

hist(solar.boot, legend="separate")

# Bootstrap Confidence Intervals
confint(solar.boot, type = "norm")

# Partial F-Test
fit2 <- lm(`Solar Cost` ~ `Roof sqft Exposed`+`Yr. Hours Sunlight` + `Property Group`, data = SolarCost)
fit2Reduced <- lm(`Solar Cost` ~ `Roof sqft Exposed`+`Yr. Hours Sunlight`, data = SolarCost)
anova(fit2Reduced, fit2)

# Is there model interaction based on Property Group? 
library(ggplot2)
library(ggthemes)
ggplot(SolarCost) + geom_point(aes(y = `Roof sqft Exponsed`, x = `Solar Cost`, color = `Property Type`))
ggplot(SolarCost) + geom_point(aes(y = `Roof sqft Exposed`, x = `Solar Cost`, color = `Property Group`)) + geom_smooth(aes(y = `Roof sqft Exposed`, x = `Solar Cost`, color = `Property Group`), method = "lm")
