# Procedure 5.4 Assessing central tendency

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions from the psych package.
# To install the package, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages("psych")


#########################################
#   The mean() and median() functions   #
#########################################

mean(qci$accuracy, na.rm = TRUE)
median(qci$accuracy, na.rm = TRUE)


# The summary() function returns the mean and the median along with other statistics
#     One variable

summary(qci$accuracy)

#     Or for multiple variables

summary(qci[, 5:9])

# The describe() function in the psych package also returns a set of statitics

library(psych)
describe(qci[, 5:10])

#     With listwise deletion

describe(qci[, 5:10], na.rm = FALSE)

# Note that the describe() function returns a trimmed mean.
# By default, 0.1 of the cases are trimmed off the top and the bottom of the distribution.
# However, the proportion to be trimmed can be changed.

describe(qci[, 5:10], trim = .05)
detach(package:psych)

# Mode - There are packages that will return the mode, 
# but like SPSS, most will only ever report one mode even when several are present.
# A simple way to obtain the mode is to tabulate using the table() function,
# then obtain the mode (or modes) from the table. 

table(qci$jobsat)
table(qci$workcond)

# The mfv() function of the modeest package reports multiple modes,
# but its argument must be numeric.
library(modeest)
mfv(qci$jobsat)
mfv(qci$workcond)
detach(package:modeest)

