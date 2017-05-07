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

widthlen = c(4,4,4,4,3,5,4,4,3,5,4,4) # set up fixed width file format
nasa1.dat = read.fwf("http://www.stat.ufl.edu/~winner/data/nasa1.dat", widthlen,
                     sep="\t", header = FALSE)
nasa1.dat = nasa1.dat[,c(2,4,6,8,10,12)] # pull out columns with data
names(nasa1.dat) = c("nozarea", "propmix",
                     "vacthrustco", "vacthrusteff",
                     "vacimpulse", "specimpulse")

summary(nasa1.dat)
pairs(nasa1.dat) # let's look at a correlation matrix
p = ggplot(nasa1.dat, aes(x=vacthrusteff, y=specimpulse)) + geom_point()
p
