# Procedure 6.2 Assessing association in contingency tables

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions 
# from the vcd, descr, rpartOrdinal, rapport and rms packages.
# To install the packages, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages(c("vcd", "descr", "rpartOrdinal", "rapport", "rms"))


# Tables involving dichotomous variables 
# and tables involving variables whose categories are nominal.
#########################
#   Tables 6.5 & 6.6    #
#########################

library(descr)
CrossTable(qci$educlev, qci$gender)
CrossTable(qci$educlev, qci$company)
detach(package:descr)


########################
#   Phi coefficient    #
########################

library(vcd)
mytable = table(qci$educlev, qci$gender)
mytable
assocstats(mytable)


############################
#   Cramer's V statistic   #
############################

mytable = table(qci$educlev, qci$company)
mytable
assocstats(mytable)

detach(package:vcd)


##############################
#   PRE statistic: lambda    #
##############################

mytable = table(qci$educlev, qci$company  )
mytable
library(rapport)
lambda.test(mytable)
detach(package:rapport)

# Check the version of rapport installed from CRAN.
# Version 0.4.0 contains an error in the calculation of lambda,
# but the authors have the correct calculation in their Github repository
# (https://github.com/Rapporter/rapport).
# While waiting for the authors to update the CRAN version,
# you can run the following lines of code.
# The code is the lambda.test function from the Github version:

lambda.test <- function(table, direction = 0) {

    if (!is.numeric(direction))
        stop('direction should be an integer between 0 and 2')
    if (!direction %in% c(0, 1, 2))
        stop('direction should be an integer between 0 and 2')

    if (direction != 0)
        (sum(as.numeric(apply(table, direction, max))) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table)))) / (sum(table) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table))))
    else
        list(row=lambda.test(table, 1), col=lambda.test(table, 2))
}

# then obtain the lambda measures:
mytable = table(qci$educlev, qci$company  )
mytable
lambda.test(mytable)

# Alternatively, you can install the GitHub version.
# If you’re running R on Windows, you need to install Rtools 
# (available from http://cran.stat.ucla.edu/bin/windows/Rtools/).
# Installation of packages from Github is straightforward using the devtools package:

install.packages("devtools")
library(devtools)
install_github('rapport', 'rapporter')

# Then obtain the lambda measures:

mytable = table(qci$educlev, qci$company  )
mytable
library(rapport)
lambda.test(mytable)
detach(package:rapport)



# Tables involving variables whose categories are ordinal.
###################
#   Tables 6.7    #
###################

cuts = quantile(na.omit(qci$speed), probs = c(.2,.4,.6,.8), type = 6)
breaks =  c(min(na.omit(qci$speed)), cuts, max(na.omit(qci$speed)))
qci$cat_speed = cut(qci$speed, breaks = breaks, right = FALSE, include.lowest = TRUE)
table(qci$cat_speed, qci$educlev)


########################
#   Gamma statistic    #
########################

library(rpartOrdinal)
ordinal.gamma(qci$educlev, qci$cat_speed)
detach(package:rpartOrdinal)


############################
#   Somer's D statistic    #
############################

library(rms)
# The function can in addition return the Gamma statistic.

# Predicting educlev from cat_speed:
res1 <- lrm(as.numeric(qci$educlev) ~ as.numeric(qci$cat_speed))
res1$stats[c("Dxy", "Gamma")]

# Predicting cat_speed from educlev:
res2 <- lrm(as.numeric(qci$cat_speed) ~ as.numeric(qci$educlev))
res2$stats[c("Dxy", "Gamma")]


########################
#   Kendall's tau-c    #
########################

# As far as I know, there is no function in any of the packages 
# for calculating Kendall's tau-c.
# I could write my own, but a Google search reveals that Douglas Y'barbo
# has already written the function and posted the result on Stackoverflow: 
# http://stackoverflow.com/questions/2567869#2567869

# To calculate Kendall's tau-c
# run the following lines of code:
################################################################
# number of concordant pairs 
P = function(t) {   
  r_ndx = row(t)
  c_ndx = col(t)
  sum(t * mapply(function(r, c){sum(t[(r_ndx > r) & (c_ndx > c)])},
    r = r_ndx, c = c_ndx))}

# number of discordant pairs
Q = function(t) {
  r_ndx = row(t)
  c_ndx = col(t)
  sum(t * mapply( function(r, c){
      sum(t[(r_ndx > r) & (c_ndx < c)])
  },
  r = r_ndx, c = c_ndx) )
}

# to calculate tau-c
kendall_tau_c = function(t){
    t = as.matrix(t) 
    m = min(dim(t))
    n = sum(t)
    ks_tauc = (m*2 * (P(t)-Q(t))) / ((n^2)*(m-1))
}
########################################################################

# then to obtain the tau-c value for the educlev ~ cat_speed table:
kc = kendall_tau_c(table(qci$educlev, qci$cat_speed))
kc
