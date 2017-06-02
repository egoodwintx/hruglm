# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#

library(ggplot2)
library(dplyr)
library(mboost)
library(corrgram)
library(lmtest)
library(broom)

# #### Dataset Description ####
# Dataset:  nasa1.dat
#
# Source: R.S. Jankovsky, T.D. Smith, A.J. Pavli (1999). "High-Area-Ratio Rocket
# Nozzle at High Combustion Chamber Pressure-Experimental and Analytical
# Validation", NASA/TP-1999-208522
#
# Description: Experiments with nozzle experiment area ratio and propellant
# mixture ratios in rockets
#
# Variables/Columns
# Nozzle Area Ratio  5-8
# Propellant Mixture Ratio  (O/F)  13-16  /* Oxidizer flow / fuel flow  */
#   Vacuum Thrust Coefficient  20-24
# Vacuum Thrust Efficiency (%)  29-32
# Vacuum Specific Impulse    36-40
# Specific Impulse Efficiency  (%)   45-48
# #### End Dataset Description ####

bonds.dat = read.csv("http://www.stat.tamu.edu/~sheather/book/docs/datasets/bonds.txt", sep='\t')
p = ggplot(data = bonds.dat, aes(CouponRate, BidPrice)) +
  geom_point() +
  ggtitle("BidPrice vs CouponRate")
p

widthlen = c(4,4,4,4,3,5,4,4,3,5,4,4) # set up fixed width file format
nasa1.dat = read.fwf("http://www.stat.ufl.edu/~winner/data/nasa1.dat", widthlen,
                     sep="\t", header = FALSE)
nasa1.dat = nasa1.dat[,c(2,4,6,8,10,12)] # pull out columns with data
names(nasa1.dat) = c("nozarea", "propmix",
                     "vacthrustco", "vacthrusteff",
                     "vacimpulse", "specimpulse")

## Let's explore the data...summary stats, correlation matrix, etc.
summary(nasa1.dat)
pairs(nasa1.dat) # let's look at a correlation matrix
corrgram(nasa1.dat)
attach(nasa1.dat)
hist(nasa1.dat$specimpulse)
plot(vacimpulse ~ nozarea, propmix)

# p = ggplot(nasa1.dat, aes(x=vacthrusteff, y=specimpulse)) + geom_point()
# p

# linear models
nasalm_1 = lm(data = nasa1.dat,
              specimpulse ~ nozarea + propmix + vacthrustco)

nasalm_2 = lm(data = nasa1.dat,
              specimpulse ~ nozarea + propmix)
summary(nasalm_1)
summary(nasalm_2)

nasalm_3 = lm(data = nasa1.dat,
              vacthrustco ~ nozarea + propmix)
summary(nasalm_3)


nasalm_4 = lm(data = nasa1.dat,
              specimpulse ~ .)
summary(nasalm_4)

## diagnostic plots
plot(resid(nasalm_3))
par(mfrow=c(2,2))
plot(nasalm_3, which=1:4)


## F-test, assumptions (normality)
# ordinary least squares vs. generalized linear models


dev.off()
## make into ggplot 2 graph
simple.df = data.frame(x = c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187),
                       y = c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92))
mod1 <- lm(y ~ x, data = simple.df)

p = ggplot(simple.df, aes(x, y))
p_point = p + geom_point()
p_point

p_abline = p_point + geom_abline(aes(intercept = mod1$coefficients[1], slope = mod1$coefficients[2]))
p_abline

p_abline_multi = p_abline +
  geom_abline(aes(intercept = -112, slope = 1.090), colour = "green") +
  geom_abline(aes(intercept = -65, slope = 0.82), colour = "red") +
  geom_abline(aes(intercept = -96, slope = 0.90), colour = "blue") +
  geom_abline(aes(intercept = -90, slope = 1.05), colour = "orange")
p_abline_multi

res = residuals(mod1)
pre = predict(mod1)

p_seg = p_abline  + geom_segment(aes(x = x, y = y, xend = x, yend = pre), colour="red")
p_seg

# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")

# add labels (res values) to points
library(calibrate)
textxy(x, y, res, cx=0.7)


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

