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
mytable <- table(qci$educlev, qci$gender)
mytable
assocstats(mytable)


############################
#   Cramer's V statistic   #
############################

mytable <- table(qci$educlev, qci$company)
mytable
assocstats(mytable)

detach(package:vcd)


##############################
#   PRE statistic: lambda    #
##############################

mytable <- table(qci$educlev, qci$company  )
mytable
library(rapport)
lambda.test(mytable)
detach(package:rapport)




# Tables involving variables whose categories are ordinal.
###################
#   Tables 6.7    #
###################

cuts <- quantile(na.omit(qci$speed), probs = c(.2,.4,.6,.8), type = 6)
breaks <-  c(min(na.omit(qci$speed)), cuts, max(na.omit(qci$speed)))
qci$cat_speed <- cut(qci$speed, breaks = breaks, right = FALSE, include.lowest = TRUE)
table(qci$cat_speed, qci$educlev)


########################
#   Gamma statistic    #
########################

library(rpf)
mat = table(qci$cat_speed, qci$educlev)
ordinal.gamma(mat)
detach(package:rpf)


############################################
#   Somer's D statistic & Gamma statistic  #
############################################

library(rms)

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
# has already written the function and made it available on Stackoverflow: 
# http://stackoverflow.com/questions/2567869#2567869

# To calculate Kendall's tau-c
# run the following lines of code:
################################################################
# number of concordant pairs 
P <- function(t) {   
  r_ndx = row(t)
  c_ndx = col(t)
  sum(t * mapply(function(r, c){sum(t[(r_ndx > r) & (c_ndx > c)])},
    r = r_ndx, c = c_ndx))}

# number of discordant pairs
Q <- function(t) {
  r_ndx = row(t)
  c_ndx = col(t)
  sum(t * mapply( function(r, c){
      sum(t[(r_ndx > r) & (c_ndx < c)])
  },
  r = r_ndx, c = c_ndx) )
}

# to calculate tau-c
kendall_tau_c <- function(t){
    t = as.matrix(t) 
    m = min(dim(t))
    n = sum(t)
    ks_tauc = (m*2 * (P(t)-Q(t))) / ((n^2)*(m-1))
}
########################################################################

# then to obtain the tau-c value for the educlev ~ cat_speed table:
kc <- kendall_tau_c(table(qci$educlev, qci$cat_speed))
kc
