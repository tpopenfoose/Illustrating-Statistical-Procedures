# Procedure 6.4 Multiple regression & prediction

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
# See READMEdata

# The R code used in the Procedure uses functions 
# from the ggplot2 package.
# To install the package, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages("ggplot2")



###################
#   Figure 6.11    #
###################

# The plot is drawn using .







###################
#   Figure 6.12    #
###################

# The plot is drawn using  .





#################################
#   Multiple Regression    #
#################################

# Summary of linear regression
m1 <- lm(accuracy ~ workcond + jobsat, data = qci)
summary(m1)

# The regression parameters are given under "Estimates"
# part way down the output, 
# and the R-square is given by "Multiple R-squared"
# one line from the bottom of the table.

# Again, the coefficients do not agree with those in ISP.


m2 <- lm(accuracy ~ speed + mentabil, data = qci)
summary(m2)

m3 <- lm(scale(accuracy) ~ scale(speed) + scale(mentabil), data = qci)
summary(m3)



cor(qci[, c(6, 8, 9)], use = "complete.obs")

library(ppcor)

pcor(na.omit(qci[, c(6, 8, 9)]))
