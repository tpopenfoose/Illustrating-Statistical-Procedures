# Procedure 5.4 Assessing central tendency


# Set the working directory

setwd("~/Cooksey")

# Read in the data file

qci = read.csv("QCI.csv", header = TRUE)

# Three variables are not numeric; they are factors.
# 'Factor' is R's way of dealing with nominal or categorical variables
# The three variables are: company, educlev, and gender.
# Setting up the factors:

qci$company = factor(qci$company)
qci$inspector = factor(qci$inspector)
qci$educlev = factor(qci$educlev)
qci$gender = factor(qci$gender)

# and change their labels from numbers to something more meaningful

levels(qci$company) = c("PC", "LEA", "SEA", "LBC", "Auto")
levels(qci$educlev) = c("High School", "Tertiary or Vocational")
levels(qci$gender) = c("Male", "Female")


#########################################
#   The mean() and median() functions   #
#########################################

mean(qci$accuracy, na.rm = TRUE)
median(qci$accuracy, na.rm = TRUE)


# The summary() function returns the mean and the median along with other statistics
#     one variable

summary(qci$accuracy)

#     or for multiple variables

summary(qci[, 5:9])

# The describe() function in the psych package also returns a set of statitics

library(psych)
describe(qci[, 5:10])

# With listwise deletion

describe(qci[, 5:10], na.rm = FALSE)

# Note that the describe() function retures a trimmed mean.
# By default, 0.1 of the cases are trimmed off the top and the bottom of the distribution.
# However, the proportion to be trimmed can be changed.

describe(qci[, 5:10], trim = .05)
detach(package:psych)

# Mode - There are packages that will return the modes, 
# but like SPSS, most will only ever report one mode even when several are present.
# A simple way to obtain the mode is to tabulate using the table() function,
# then obtain the mode (or modes) from the table. 

table(qci$jobsat)
table(qci$workcond)
