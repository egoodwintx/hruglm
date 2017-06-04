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

######### THE BOND DATASET ########
bonds.dat = read.csv("http://www.stat.tamu.edu/~sheather/book/docs/datasets/bonds.txt", sep='\t')
p = ggplot(data = bonds.dat, aes(CouponRate, BidPrice)) +
  geom_point() +
  ggtitle("BidPrice vs CouponRate")
p

bondmodel = lm(BidPrice ~ CouponRate, data = bonds.dat)

pbond = p + geom_abline(aes(intercept = bondmodel$coefficients[1],
                            slope = bondmodel$coefficients[2]),
                        colour = "green") +
  ggtitle("BidPrice vs CouponRate\n with Regression Line")
pbond
res = residuals(bondmodel)
pre = predict(bondmodel)
p_seg = pbond  + geom_segment(aes(x = CouponRate, y = BidPrice, xend = CouponRate, yend = pre), colour="red")
p_seg

## eliminate three outliers
bondsclean.dat = bonds.dat[-c(4, 13, 35),]
bondcleanmodel = lm(BidPrice ~ CouponRate, data = bondsclean.dat)
p = ggplot(data = bondsclean.dat, aes(CouponRate, BidPrice)) +
  geom_point() +
  ggtitle("BidPrice vs CouponRate")
p


pbond = p + geom_abline(aes(intercept = bondcleanmodel$coefficients[1],
                            slope = bondcleanmodel$coefficients[2]),
                        colour = "green") +
  ggtitle(" Vanilla Bond")
pbond
res = residuals(bondcleanmodel)
pre = predict(bondcleanmodel)
p_seg = pbond  + geom_segment(aes(x = CouponRate, y = BidPrice,
                                  xend = CouponRate, yend = pre),
                              colour="red")
p_seg

## clear the plots
dev.off()

## Advertising data set from ISLR
ad.dat = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")

## Let's look at the ad data
summary(ad.dat)
pairs(ad.dat)
corrgram(ad.dat)

sales.mod = lm(data = ad.dat, Sales ~ TV + Newspaper + Radio)
tidy(sales.mod)
pairs(ad.dat)
corrgram(ad.dat)

## F-test, assumptions (normality)






# ## Presentation
# lm(formula, data, subset, weights, na.action,
#    method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
#    singular.ok = TRUE, contrasts = NULL, offset, ...)
#
## what is a linear model?
## when can we use it? quantitative, normal data
## what is the lm formula and its inputs?
## what are the downsides to this model? relatively inflexible and simple
# Given that estimation is undertaken on the basis of a least
# squares analysis, estimates of the unknown parameters βj are determined by
# minimising a sum of squares function {\displaystyle S=\sum
# _{i=1}^{n}\left(Y_{i}-\beta _{0}-\beta _{1}\phi _{1}(X_{i1})-\cdots -\beta
# _{p}\phi _{p}(X_{ip})\right)^{2}.} S=\sum _{{i=1}}^{n}\left(Y_{i}-\beta
# _{0}-\beta _{1}\phi _{1}(X_{{i1}})-\cdots -\beta _{p}\phi
# _{p}(X_{{ip}})\right)^{2}. From this, it can readily be seen that the "linear"
# aspect of the model means the following: the function to be minimised is a
# quadratic function of the βj for which minimisation is a relatively simple
# problem; the derivatives of the function are linear functions of the βj making
# it easy to find the minimising values; the minimising values βj are linear
# functions of the observations Yi; the minimising values βj are linear
# functions of the random errors εi which makes it relatively easy to determine
# the statistical properties of the estimated values of βj.

