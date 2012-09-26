# Procedure 6.1 Assessing correlation

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions 
# from the psych and polycor packages.
# To install the packages, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages(c("psych", "polycor"), dependencies = TRUE)


# Simple pearson r correlation

cor(qci$accuracy, qci$workcond, use = "pairwise")

# Spearman rho correlation

cor(qci$accuracy, qci$workcond, method = "spearman", use = "pairwise")

# Kendall tau correlation (related to spearman correlation)

cor(qci$accuracy, qci$workcond, method = "kendall", use = "pairwise")


# In P5_3, a SPLOM was drawn:
# with scatterplots in the lower half of the matrix;
# correlations were in the upper half;
# and histograms along the diagonal.
# Two SPLOMs are drawn here. The scatterplots do not show the points but instead
# correlation ellipses are drawn. 

library(psych)
pairs.panels(qci[, 10:18], scale = FALSE, smooth = FALSE, show.point = FALSE)
pairs.panels(qci[, 6:9], scale = FALSE, smooth = FALSE, show.point = FALSE)
detach(package:psych)


########################
#   Tables 6.1 & 6.2   #
########################

round(cor(qci[, 6:9], method = "pearson", use = "pairwise"), 3)  # Table 6.1
round(cor(qci[, 6:9], method = "spearman", use = "pairwise"), 3) # Table 6.2


# The last nine variables in qci are measured using on 7-point scales.
# It might me reasonable to assume that the variables represent 
# unobserved continuous normal variables.
# If so, then polychoric correlations are appropriate. 
# Polychoric are like tetrachoric except the variables in polychoric correlations 
# have more than two categories.

library(psych)
round(polychoric(qci[, 10:18])$rho, 3)

# Compared to person correlations.

round(cor(qci[, 10:18], method = "pearson", use = "pairwise"), 3)


#################################
#   biserial & point-biserial   #
#################################

# biserial - correlation between x and dichotomous y but y is assumed 
# to represent an unobserved continuous normal variable.

round(biserial(qci[, 6:9], qci$educlev), 3)

# point-biserial - pearson correlation between x and dichotomous y,
# as shown is Table 6.3

round(cor(qci[, 6:9], as.numeric(qci$educlev), method = "pearson", use = "pairwise"), 3)

detach(package:psych)


# Fox J (2009). polycor: Polychoric and Polyserial Correlations. R package version 0.7-7, URL
# http://CRAN.R-project.org/package=polycor.