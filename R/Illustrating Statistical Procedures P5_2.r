# Procedure 5.2 Graphical Methods for Displaying Data


# There are many graphics outputs to follow.
# At the first blank window, right-click or ENTER to obtain the first output,
# then right-clisk or ENTER to progress through the outputs.
# BUT NOTE, three graphs require other interactive input - Figures 5.5, 5.8 & 5.9.

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
qci$inspector = factor(qci$inspector)
qci$educlev = factor(qci$educlev)
qci$gender = factor(qci$gender)

# and change their labels from numbers to something more meaningful

levels(qci$company) = c("PC", "LEA", "SEA", "LBC", "Auto")
levels(qci$educlev) = c("High School", "Tertiary or Vocational")
levels(qci$gender) = c("Male", "Female")


#############################
#   BARPLOT  (Figure 5.1)   #
#############################

# First, obtain a table of percentages of female inspectors

xF = table(qci$company[qci$gender == "Female"])
xpF = xF/sum(xF) * 100

# A basic barplot

barplot(xpF)

# Add some annotations

barplot(xpF, las = 1, axis.lty = 1, xlab = "Company Type", ylab = "Percent", ylim = c(0,30))
box()

# Add more annotations

company.labels = c("PCs\n", "Large electrical\nappliances", "Small electrical\nappliances",
   "Large business\ncomputers", "Automobiles\n")
barplot(xpF, las = 1, axis.lty = 1, names.arg = company.labels, cex.names = .8, 
   xlab = "Company Type", ylab = "Percent", ylim = c(0,30))
box()
title("Figure 5.1 Barchart: Percentage of female inspectors", line = 1)


##############################
#   PIE CHART (Figure 5.2)   #
##############################

names(xpF) = c("PCs\n", "Large electrical\nappliances", "Small electrical\nappliances",
   "Large business\ncomputers", "Automobiles\n")
pie(xF, labels = names(xpF), col = gray(seq(0.4, 0.9, length=5)))
title("Figure 5.2 Pie chart: Percentage of female inspectors", line = 0)


##############################
#   HISTOGRAM (Figure 5.3)   #
##############################

hist(qci$speed, breaks = seq(-1, 21, 2), xlab = "Speed", xaxt = "n", col = "grey", 
     ylim = c(0,50), las = 1, main = "")
box()
axis(1, at = seq(0, 20, 2), labels = seq(0, 20, 2))
title(main = list(expression(paste("Figure 5.3 Histogram of the ", 
       italic("speed"), " variable"))), line = 1)


######################################
#   FREQUENCY POLYGON (Figure 5.4)   #
######################################

# Frequency polygons are not often drawn, and there is no function 
# for doing so in the base packages.
# However there is the simple.freqpoly() function in the UsingR package

library(UsingR)
simple.freqpoly(qci$speed, breaks = seq(-1, 21, 2), xlab = "Speed", 
   border = NA, col = NA, ylim = c(0,50), las = 1, main = "")
box()
title(main = list(expression(paste("Figure 5.3 Frequency polygon plot of the ", 
   italic("speed"), " variable"))), line = 1)
detach(package:UsingR)

# More usual is to draw a density plot (a smoothed frequency polygon)

plot((density(qci$speed, na.rm = TRUE)), main = "", xlab = "Speed")
title(main = list(expression(paste("Figure 5.3a Density polygon plot of the ", 
   italic("speed"), " variable"))), line = 1)

# Or a histogram with a density plot superimposed

hist(qci$speed, breaks = seq(-1, 21, 2), xlab = "Speed", xaxt = "n", col = "grey", 
   main = "", prob = TRUE)
lines(density(qci$speed, na.rm = TRUE))
axis(1, at = seq(0, 20, 2), labels = seq(0, 20, 2))
title(main = list(expression(paste("Figure 5.3b Histogram and Density plot of the ", 
   italic("speed"), " variable"))), line = 1)
box()


###########################################
#  BACK-TO-BACK HISTOGRAMS (Figure 5.5)   #
###########################################

# There is a histbackback() function in the Hmisc package
# then two barplots are drawn.
# The locator argument in the text() function allows labels for education level
# to be located in the appropiate half.
# But you do need to know which side is which.
# 'High school' is on the left.
# After running the text() function, click first in the left barplot, 
# then click in the right barplot.


library(Hmisc)
options(digits = 1)
op = par(las = 1)
hbb = histbackback(split(qci$speed, qci$educlev), xlim = c(-30, 30), 
   xlab = "Count", ylab = "Speed")
barplot(-hbb$left, col = "light grey", horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
barplot(hbb$right, col = "dark grey", horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
text(locator(2), labels = c("High School", "Tertiary / Vocational")) 
title(main = list(expression(paste("Figure 5.5 Back-to-back histograms of ", 
   italic("speed")))), line = 2)
title(main = list(expression(paste("for the two categories of the ", 
   italic("educlev"), " variable"))), line = 1)
par(op)
options(digits = 7)
detach(package:Hmisc)

# An alternative is to draw two density plots

x = qci$speed[qci$educlev == "High School"]
x = x[is.na(x) == 0]
y = qci$speed[qci$educlev == "Tertiary or Vocational"]
y = y[is.na(y) == 0]
plot(density(x), col = "red", main = "", xlab = "Speed")
lines(density(y), col = "blue")
legend(10, 0.15, legend = c("High School", "Tertiary or Vocational"), 
   pch = c(15, 15), col = c("red", "blue"))
title(main = list(expression(paste("Figure 5.5a Density plots of ", 
   italic("speed")))), line = 2)
title(main = list(expression(paste("for the two categories of the ", 
   italic("educlev"), " variable"))), line = 1)


##############################
#   LINE GRAPH (Figure 5.6)  #
##############################

# Obtain a table of means

xbar = tapply(qci$accuracy, qci$company, mean, na.rm = TRUE)
xbar

plot(1:5, xbar, type = "b", xaxt = "n", ylim = c(70, 95), las = 1, 
   xlab = "Company Type", ylab = "Mean Inspection Accuracy (%)")
axis(1, at = 1:5, labels = names(xbar))
title(main = list(expression(paste("Figure 5.6 Line graph comparison of companies in terms of "))),
  line = 2)
title(main = list(expression(paste("average inspection ", italic("accuracy")))), line = 1)


#########################################################
#   LINE GRAPH WITH CONFIDENCE INTERVALS (Figure 5.7)   #
#########################################################

# Confidence intervls can be drawn with difficulty using functions available in the 
# base packages, but it is easier to use plotmeans() function in the gplots package.

library(gplots)
op = par(
      las = 1,
	  mar = c(5,4,4,2) + .1,
	  mgp = c(2.5, .5, 0)
	  )
plotmeans(qci$accuracy ~ qci$company, ylim = c(70, 95), xlab = "Company Type", 
   ylab = "Mean Inspection Accuracy (%) with 95% CIs", n.label = FALSE, use.t = FALSE)
title(main = list(expression(paste("Figure 5.7 Line graph using confidence interval bars to "))), 
  line = 2)
title(main = list(expression(paste("compare ", italic("accuracy"), " across companies"))), line = 1)
par(op)
detach(package:gplots)


################################
#   SCATTERPLOT (Figure 5.8)   #
################################

# The identify() function labels each point with the inspector's number 
# with each click of the left mouse button.
# Right-click when finished.

plot(qci$speed, qci$accuracy, pch = 20, xlim = c(0, 20), ylim = c(50, 100), las = 1, 
   xlab = "SPEED (seconds)", ylab = "ACCURACY (percent)")
title(main = list(expression(paste("Figure 5.8 Scatterplot relating inspection ", 
   italic("accuracy"), " to inspection ", italic("speed")))), line = 1)
identify(qci$speed, qci$accuracy, labels = qci$inspector)


############################################
#   CONDITIONAL SCATTERPLOT (Figure 5.9)   #
############################################

# The identify function is used again.
# As before, right-click when finished.

plot(qci$speed, qci$accuracy, pch = as.numeric(qci$educlev) + 19, 
   xlim = c(0, 20), ylim = c(50, 100), las = 1, 
   xlab = "SPEED (seconds)", ylab = "ACCURACY (percent)")
legend(15, 60, legend = c("High school", "Tertiary"), pch = c(1,2) + 19)
title(main = list(expression(paste("Figure 5.9 Scatterplot displaying ", 
  italic("accuracy"), " vs ", italic("speed")))), line = 2)
title(main = list(expression(paste("conditional on ", 
  italic("educlev"), " groups"))), line = 1)
identify(qci$speed, qci$accuracy, labels = qci$inspector)

devAskNewPage(ask=FALSE)