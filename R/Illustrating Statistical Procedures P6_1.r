# Procedure 6.1 Assessing correlation


# There are many graphics outputs to follow.
# At the first blank window, right-click or ENTER to obtain the first output,
# then right-clisk or ENTER to progress through the outputs.

devAskNewPage(ask=TRUE)

# Set the working directory
setwd("~/Cooksey")

# Read in the data file
qci = read.csv("QCI.csv", header = TRUE)

# Three variables are not numeric; they are factors.
# 'Factor' is R's way of dealing with nominal or categorical variables
# The three variables are: company, educlev, and gender.
# Setting up the factors:

qci$company = factor(qci$company)
qci$educlev = factor(qci$educlev)
qci$gender = factor(qci$gender)

# and change their labels from numbers to something more meaningful

levels(qci$company) = c("PC", "LEA", "SEA", "LBC", "Auto")
levels(qci$educlev) = c("High School", "Tertiary or Vocational")
levels(qci$gender) = c("Male", "Female")

# Simple pearson r correlation

cor(qci$accuracy, qci$workcond, use = "pairwise")

# Spearman fho correlation

cor(qci$accuracy, qci$workcond, method = "spearman", use = "pairwise")

# Kendall tau correlation (related to spearman correlation)

cor(qci$accuracy, qci$workcond, method = "kendall", use = "pairwise")


# In P_2_5_3, a SPLOM was drawn:
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

devAskNewPage(ask=FALSE)
