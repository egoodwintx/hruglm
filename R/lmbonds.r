require(ggplot2)
require(dplyr)
require(corrgram)
require(lmtest)
require(broom)
require(ISLR)

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
