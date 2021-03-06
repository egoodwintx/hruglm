##
## [nasamodel.r]
##
## author     : Ed Goodwin
## project    : hruglm
## createdate : 06.03.2017
##
## description:
##    linear models in R…HRUG presentation
##
## version: 0.01
## changelog:
##


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
