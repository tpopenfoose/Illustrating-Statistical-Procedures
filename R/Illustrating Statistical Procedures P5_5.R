# Procedure 5.5 Assessing variability

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions 
# from the psych and plyr packages.
# To install the packages, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages(c("psych", "plyr"), dependencies = TRUE)


################################
#   range, maximum & minimum   #
################################

# The range is obtained using the range() function,
# but note the the function returns the maximum and minimum values.
# The range in then obtained be subtraction.
# Note also, the max() and min() functions also return
# the maximum and minimum values.

(min.max = range(qci$speed, na.rm = TRUE))
min.max[2] - min.max[1]

(x.min = min(qci$speed, na.rm = TRUE))
(x.max = max(qci$speed, na.rm = TRUE))
x.max - x.min


################################################
#   quartiles, interquartile range, & hinges   #
################################################

# The IQR is obtained using the IQR() function.

IQR(qci$speed, na.rm = TRUE)

#      Or from the quartiles.

(q1 = quantile(qci$speed, 0.25, na.rm = TRUE))
(q3 = quantile(qci$speed, 0.75, na.rm = TRUE))
q3 - q1

#     In R, there are nine ways to position the quantiles. 
#     The answers using R's default method do not agree with the values
#     given in Procedure 5.5, Table 5.4.
#     SPSS and Minitab use type 6; SAS uses type 3;
#     R's default is type 7.

quantile(qci$speed, prob = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 6) # SPSS
quantile(qci$speed, prob = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 3) # SAS
quantile(qci$speed, prob = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 7) # R default

#     Note that the 3rd and 4th quartiles given by the summary() function use R's default.

summary(qci$speed)

#     The fivenum() function returns Tukey's five number summary:
#     minimum, lower hinge, median, upper hinge, maximum.
#     Note that the hinges do not necessarily equal the 3rd and 4th quartiles,
#     although they will be close.  

fivenum(qci$speed, na.rm = TRUE)


#####################################
#   variance & standard deviation   #
#####################################

# Varaince is given by the var() function, 
# Standard devaition is given by the sd() function

var(qci$speed, na.rm = TRUE)
sd(qci$speed, na.rm = TRUE)

# A measure similar to the standard deviation is the median absolute deviation 
# from the median, adjusted by a factor of 1.4826 so that it is approximately 
# equal to the standard deviation of a normally distributed sample.

mad(qci$speed, na.rm = TRUE)


#############################
#   Additional statistics   #
#############################

# Two additional statistics, mentioned in Cooksey on pp. 6 & 7,
# are skewness and kurtosis.
# They are given by the describe() function in the psych package.
# (Note that describe() also returns standard deviation, mad, and the range.)

library(psych)
describe(qci$speed)
describe(qci[, 5:9])
detach(package:psych)


###################################################
#   Design your own table of summary statistics   #
###################################################

# The following returns a table of selected summary statistics for speed, 
# for each company type. 

library(plyr)
library(psych)
qci$All = "All"
(SummaryTable1 <- ddply(qci, 'All', summarise,
              Mean = round(mean(speed, na.rm = TRUE), 2),
              Median = round(median(speed, na.rm = TRUE), 2),
              `St Dev` = round(sd(speed, na.rm = TRUE), 2),
              Min = min(speed, na.rm = TRUE),
              Max = max(speed, na.rm = TRUE),
              `valid N` = sum(!is.na(speed)),
              Missing = sum(is.na(speed)),
              Skew = round(skew(speed, na.rm = TRUE), 2),
              Kurtosis = round(kurtosi(speed, na.rm = TRUE), 2),
              se = round(describe(speed)$se, 2)))

(SummaryTable2 <- ddply(qci, 'company', summarise,
              Mean = round(mean(speed, na.rm = TRUE), 2),
              Median = round(median(speed, na.rm = TRUE), 2),
              `St Dev` = round(sd(speed, na.rm = TRUE), 2),
              Min = min(speed, na.rm = TRUE),
              Max = max(speed, na.rm = TRUE),
              `valid N` = sum(!is.na(speed)),
              Missing = sum(is.na(speed)),
              Skew = round(skew(speed, na.rm = TRUE), 2),
              Kurtosis = round(kurtosi(speed, na.rm = TRUE), 2),
              se = round(describe(speed)$se, 2)))

names(SummaryTable1)[1] = "company"

SummaryTable = rbind(SummaryTable1, SummaryTable2)
SummaryTable
