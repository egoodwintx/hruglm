##
## [lmadvertising.r]
##
## author     : Ed Goodwin
## project    : hruglm
## createdate : 06.04.2017
##
## description:
##    advertising model for HRUG linear regression model
##
## version: 0.01
## changelog:
##
require(ggplot2)
require(dplyr)
require(corrgram)
require(broom)
require(ISLR)
require(gridExtra)
require(MASS)

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

## Advertising exploratory graphics
p_TV = ggplot(data=ad.dat, aes(x=TV, y=Sales)) + geom_point()
p_Newspaper = ggplot(data=ad.dat, aes(x=Newspaper, y=Sales)) + geom_point()
p_Radio = ggplot(data=ad.dat, aes(x=Radio, y=Sales)) + geom_point()
grid.arrange(p_TV, p_Newspaper, p_Radio, ncol=2)

p_TV = ggplot(data=ad.dat, aes(x=TV, y=boxCoxVariable(Sales))) + geom_point()
p_TV

par(mfrow = c(1,1))
grid.arrange(ncol=1)
bc = boxcox(data = ad.dat, Sales ~ TV)

sales.mod2 = lm(data = ad.dat, Sales ~ TV + Radio)
summary(sales.mod2)

sales.mod3 = lm(data = ad.dat, Sales ~ TV + TV*Radio + Radio)
summary(sales.mod3)


sales.mod4 = lm(data = ad.dat, Sales ~ TV + TV*Radio + Radio + Newspaper + Newspaper*TV)
summary(sales.mod4)

