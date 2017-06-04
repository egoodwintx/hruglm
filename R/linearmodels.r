##
## [linearmodels.r]
##
## author     : Ed Goodwin
## project    : Linear Regression HRUG Talk
## createdate : 06.02.2017
##
## description:
##    code to create plots and linear models for HRUG talk
##
## version: 0.01
## changelog:
##

library(ggplot2)
library(dplyr)
library(corrgram)
library(lmtest)
library(broom)
library(ISLR)

######### THE SIMPLE DATASET ########
simple.df = data.frame(x = c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187),
                       y = c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92))

## create linear regression model
mod1 <- lm(y ~ x, data = simple.df)

## simple scatter plot of points
p = ggplot(simple.df, aes(x, y))
p_point = p + geom_point()
p_point

## plot line of regression model on scatterplot
p_abline = p_point + geom_abline(aes(intercept = mod1$coefficients[1], slope = mod1$coefficients[2]))
p_abline

## plot multiple other lines on scatterplot to illustrate search for regression line
p_abline_multi = p_abline +
  geom_abline(aes(intercept = -112, slope = 1.090), colour = "green") +
  geom_abline(aes(intercept = -65, slope = 0.82), colour = "red") +
  geom_abline(aes(intercept = -96, slope = 0.90), colour = "blue") +
  geom_abline(aes(intercept = -90, slope = 1.05), colour = "orange")
p_abline_multi

## calc residuals and plot error lines on regression plot
res = residuals(mod1)
pre = predict(mod1)
p_seg = p_abline  + geom_segment(aes(x = x, y = y, xend = x, yend = pre), colour="red")
p_seg


## clear the plots
dev.off()



## F-test, assumptions (normality)

# ## Presentation
# lm(formula, data, subset, weights, na.action,
#    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#    singular.ok = TRUE, contrasts = NULL, offset, ...)

## what is a linear model?
## when can we use it? quantitative, normal data
## what is the lm formula and its inputs?
## what are the downsides to this model? relatively inflexible and simple
