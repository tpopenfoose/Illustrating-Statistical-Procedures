# Procedure 5.3 Multivariate graphs & displays

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
qci$inspector = factor(qci$inspector)
qci$educlev = factor(qci$educlev)
qci$gender = factor(qci$gender)

# and change their labels from numbers to something more meaningful

levels(qci$company) = c("PC", "LEA", "SEA", "LBC", "Auto")
levels(qci$educlev) = c("High School", "Tertiary or Vocational")
levels(qci$gender) = c("Male", "Female")


############################################
#   MATRIX OF SCATTERPLOTS (Figure 5.10)   #
############################################

pairs(qci[, 5:9])

# The pairs.panels() function in the psych package adds a histogram and a correlation (see Procedure 6.1)

library(psych)
pairs.panels(qci[, c(5:9)], pch = 1, scale = TRUE, smooth = FALSE, ellipses = FALSE)

# There is overplotting in the scatterplots.
# Jittering deals with the overplotting.

qci$jobsatj = jitter(qci$jobsat, factor = 1)
qci$workcondj = jitter(qci$workcond, factor = 1)
pairs.panels(qci[, c(5:7, 19:20)], pch = 1, scale = TRUE, smooth = FALSE, ellipses = FALSE)
title(main = list(expression(paste("Figure 5.10 Scatterplot matrix"))), line = 3)
detach(package:psych)


######################################################
#   RADAR PLOT / SPIDER PLOT (Figures 5.11 & 5.12)   #
######################################################

# # Radar / Spider plots can be obtained a number of ways.
# The polar.plot() function in the plotrix package is used here.

library(plotrix)
testpos = seq(0, by = 360/9, length = 9)
testlen = qci[c(66, 104), 10:18] 
polar.plot(testlen, testpos, start = 90, clockwise = TRUE, lwd = 2, rp.type = "p",  
    show.grid= TRUE, label.pos = seq(0, by = 360/9, length = 9), 
	labels = names(qci[, 10:18]), radial.lim = c(0, 7), mar = c(5,5,5,5))
legend(6,-6, legend= c("Insp 66", "Insp 104"), pch = c(15, 15), col = c("black", "red"))
title(main = "Figure 5.11 Radar plot comparing attitude ratings\nfor inspector 66 and 104")
par(mar = c(5, 4, 4, 2) + .1)
detach(package:plotrix)

company.means = aggregate(x = qci[, 10:18], by = list(company = qci$company), 
   FUN = "mean", na.rm = TRUE)
library(plotrix)
testpos = seq(0, by = 360/9, length = 9)
testlen = company.means[1:5, 2:10] 
polar.plot(testlen, testpos, start = 90, clockwise = TRUE, lwd = 2, rp.type = "p",  
    show.grid= TRUE, label.pos = seq(0, by = 360/9, length = 9), 
	labels = names(qci[, 10:18]), radial.lim = c(2, 5.5), mar = c(5,5,5,5))
legend(3,-3, legend= c("PC", "LEA", "SEA", "LBC", "Auto"), pch = rep(15, 55), 
   col = c("black", "red", "green", "blue", "cyan"))
title(main = "Figure 5.12 Radar plot comprising average attitude ratings\nfor five types of company")
par(mar = c(5, 4, 4, 2) + .1)
detach(package:plotrix)


################################################
#   MULTIPLOTS (LATTICE PLOTS) (Figure 5.13)   #
################################################

# The layout() function creates five displays on the one page.
# The plot() function plots in each display in turn.

op = par(mar = c(0,0,1.5,0),  xaxt = "n", yaxt = "n")
layout(matrix(c(1,2,3,4,5), ncol = 5), respect = TRUE)
plot(x = 1:9, y = company.means[1, 2:10], main = "PC", xlab = "", ylab = "", 
   type = "l", ylim = c(1,7))
plot(x = 1:9, y = company.means[2, 2:10], main = "LEA", xlab = "", ylab = "", 
   type = "l", ylim = c(1,7))
plot(x = 1:9, y = company.means[3, 2:10], main = "SEA", xlab = "", ylab = "", 
   type = "l", ylim = c(1,7))
plot(x = 1:9, y = company.means[4, 2:10], main = "LBC", xlab = "", ylab = "", 
   type = "l", ylim = c(1,7))
plot(x = 1:9, y = company.means[5, 2:10], main = "Auto", xlab = "", ylab = "", 
   type = "l", ylim = c(1,7))
par(op)
layout(1)

# An alternative is to plot the profiles in a single display,
# then colour-code for company types.

op = par(mar = c(7,4,4,1))
plot(x = 1:9, y = company.means[1, 2:10], type = "l", xaxt = "n", 
   ylim = c(1,7), lwd = 2, las = 1, xlab = "", ylab = "Average ratings")
axis(1, at = 1:9, labels = names(company.means)[2:10], las = 2)
title(xlab = "Attitude Variables", mgp = c(5.5,1,0))
points(x = 1:9, y = company.means[2, 2:10], lwd = 2, type = "l", col = "red")
points(x = 1:9, y = company.means[3, 2:10], lwd = 2, type = "l", col = "blue")
points(x = 1:9, y = company.means[4, 2:10], lwd = 2, type = "l", col = "green")
points(x = 1:9, y = company.means[5, 2:10], lwd = 2, type = "l", col = "cyan")
legend(1.5, 6.5, legend = company.means[1:5, 1], pch = rep(16, 5), 
   col = c("black", "red", "blue", "green", "cyan"))
par(op)

# The lattice package was designed for multiple plots on the one page

long = reshape(company.means, idvar = "company", varying = list(2:10), 
   v.names = "ratings", direction = "long", timevar = "variable")
library(lattice)

#   Horizontal

xyplot(long$ratings ~ long$variable | long$company, type = "l", 
   xlab = "Attitude Variables", ylab = "Mean Ratings",
   layout = c(5, 1), ylim = c(1, 7), aspect = c(1.2), 
   scales =list(y = list(at = 1:7), x = list(at = 1:9, rot =45, 
   labels = names(company.means)[2:10], cex = .7)))

#   Vertical

xyplot(long$variable ~ long$ratings | long$company, type = "l", 
   ylab = "Attitude Variables", xlab = "Mean Ratings",
   layout = c(1, 5), xlim = c(1, 7), aspect = c(.6), 
   scales =list(x = list(at = 1:7), y = list(at = 1:9, 
   labels = names(company.means)[2:10])))   
   
detach(package:lattice)


################################################
#   PARALLEL COORDINATES PLOTS (Figure 5.14)   #
################################################

# A number of ways to generate a parallel coordinates plot.
# Possibly the simplest is to use the parallel() function in the lattice package

library(lattice)

#   All company types

parallelplot(~qci[, 10:18] | company, data = qci, layout = c(5,1), col = "black")

#   The two companies in Figure 5.14

qci.sub = subset(qci, company == "PC" | company == "Auto")
parallelplot(~qci.sub[, 10:18] | company, data = qci.sub, layout = c(2,1), col = "black")
detach(package:lattice)


###################################################
#   ICON PLOTS (CHERNOFF'S FACES) (Figure 5.15)   #
###################################################

# The faces() function in the TeachingDemos package is demonstrated first.

# In the TeachinfDemos package:
# The facial features are: 1-height of face, 2-width of face,  
# 3-shape of face, 4-height of mouth, 5-width of mouth, 6-curve of smile, 
# 7-height of eyes, 8-width of eyes, 9-height of hair, 10-width of hair, 
# 11-styling of hair, 12-height of nose, 13-width of nose, 
# 14-width of ears, 15-height of ears. 

library(TeachingDemos)
faces(qci[c(3,4,17,40,60,66,75,86,104,111), 10:18])
detach(package:TeachingDemos)

# The symbols package allows a number of icons, including faces.
# The facial features are: 1-face width; 2-face height;
# 3-eyes size; 4-eyes distance; 5-mouth width;
# 6-mouth curve; 7-brows size; 8-brows position;
# 9-nose length; 10-nose width; 11-ears size;
# 12-pupils position.

library(symbols)
symbol(qci[c(3,4,17,40,60,66,75,86,104,111), c(1, 10:18)], labels = 1, type = "face")

#       colour according to another variable; here gender

symbol(qci[c(3,4,17,40,60,66,75,86,104,111), c(1, 4, 10:18)], type = "face", 
   labels = 1, colin = 2)


# Other icons available in the symbols package include profiles, histograms and stars.

#######################################################
#   ICON PLOTS (PROFILE PLOT) (Figures 5.16 & 5.17)   #
#######################################################

# Can be drawn using the symbols package, but there is no way to control the normalisation
# of the variables; so when plotting a small number of cases, the full range of scales is 
# less likely to appear, and so the normalisation is likely based on different maxima 
# and minima for each variable. See the two plots below.

symbol(qci[c(3,4,17,40,60,66,75,86,104,111), c(1, 10:18)], type = "profile", labels = 1)

symbol(qci[c(3,4,17,40,60,66,75,86,104,111), c(1, 10:18)], type = "bar", labels = 1)

detach(package:symbols)


###########################################
#   ICON PLOT (STAR PLOT) (Figure 5.18)   #
###########################################

# The stars() function allows independent normalisation

df = qci[c(3,4,17,40,60,66,75,86,104,111), 10:18] / 7

stars(df, scale = FALSE)

#    with a key

stars(df, scale = FALSE, key.loc = c(7.5, 2.2), len = 0.7)

#    without the radii

stars(df, scale = FALSE, radius = FALSE, key.loc = c(7.5, 2.2), len = 0.7)

#    can also plot segments; here without colours

stars(df, scale = FALSE, radius = FALSE, draw.segments = TRUE, 
   col.segments = 0, key.loc = c(7.5, 2.2), len = 0.7)

#     and with colours

stars(df, scale = FALSE, radius = FALSE, draw.segments = TRUE, 
   col.segments = rainbow(9, s = 0.7, v = 0.9), key.loc = c(7.5, 2.2), len = 0.7)

devAskNewPage(ask=FALSE)