# Procedure 5.6 Exploratory data analysis


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


###########################################
#   Stem and Leaf display (Figure 5.22)   #
###########################################

stem(qci$accuracy, scale = .5)

# The stem.leaf() function in the aplpack draws the classic Tukey-style 
# stem-and-leaf display.

library(aplpack)
stem.leaf(qci$accuracy)

# the number of leaves per stem is determined by the software.
# For this display, it was 2.
# Note the use of "*" and "." to distinguish the stems.
# It can be modified. In the next display, it is set to 1.

stem.leaf(qci$accuracy, m = 1)

#  Next it is set to 5
#  The symbols on the stems mean:
#         * - 0 & 1
#         t - 2 & 3
#         f - 4 & 5
#         s - 6 & 7
#         . - 8 & 9

stem.leaf(qci$accuracy, m = 5)
detach(package:aplpack)


#############################
#   Boxplot (Figure 5.24)   #
#############################

boxplot(qci$mentabil)

#  Add some labels & rotate y-axis tick mark labels to horizontal

boxplot(qci$mentabil, ylab = "Score", xlab = "Mental Ability", las = 1)

# Add case numbers to the outliers

text(1.1, max(qci$mentabil, na.rm = TRUE), 
   label = which(qci$mentabil == max(qci$mentabil, na.rm = TRUE)))
text(1.1, min(qci$mentabil, na.rm = TRUE), 
   label = which(qci$mentabil == min(qci$mentabil, na.rm = TRUE)))

# Use the layout() function to position two boxplots side-by-side
# as in Figure 5.24

layout(matrix(c(1,2), ncol = 2), heights = lcm(12))
boxplot(qci$mentabil, ylab = "Score", xlab = "Mental Ability", las = 1)
text(1.1, max(qci$mentabil, na.rm = TRUE), 
   label = which(qci$mentabil == max(qci$mentabil, na.rm = TRUE)), cex = .75)
text(1.1, min(qci$mentabil, na.rm = TRUE), 
   label = which(qci$mentabil == min(qci$mentabil, na.rm = TRUE)), cex = .75)

boxplot(qci$speed, ylab = "Score", xlab = "Speed", las = 1)
x = sort(qci$speed, decreasing = TRUE)
text(1.1, x[1], label = which(qci$speed == x[1]), cex = .75)
text(1.1, x[2], label = which(qci$speed == x[2]), cex = .75)
text(1.1, x[3]+0.2, label = which(qci$speed == x[3]), cex = .75)
text(1.1, x[4]-0.2, label = which(qci$speed == x[4]), cex = .75)

layout(1)


#####################################
#   Notched Boxplot (Figure 5.25)   #
#####################################

# Side-by-side boxplots

boxplot(qci$accuracy ~ qci$company)

# Side-by-side notch boxplots

boxplot(qci$speed ~ qci$company, notch = TRUE)

# Position the two plots side-by-side as in Figure 5.25

layout(matrix(c(1,2), ncol = 2), heights = lcm(12))
boxplot(qci$accuracy ~ qci$company, ylab = "Accuracy", xlab = "Company", 
   las = 1, cex.axis = 0.75)
boxplot(qci$speed ~ qci$company, notch = TRUE, ylab = "Speed", xlab = "Company",
   las = 1, cex.axis = 0.75)
layout(1)


# Sometimes, it is useful to overlay the boxplot with the data points.
# I do this fairly often, so rather than typing in the boxplot command, 
# followed by the points command, and then adjusting the options,
# I've wrapped it all up in a function called boxdata().
# This demonstrates a second way in which R is free (the first is that it is free of cost).
# If you find R's functions do not quite meet you needs, you can write your own function. 
# In the boxdata() function, I have set as default some options I like, 
# but the options can be changed.

boxdata = function(x, y, xlab = "", ylab = "", main = "", 
   ylim = c(min(y, na.rm = TRUE), max(y, na.rm = TRUE)),
   box.col = "sky blue", box.cex = .3, p.pch = 16, p.col = "red", p.cex = .75) {
   if (is.factor(x) == FALSE)
      cat(deparse(substitute(x)), " is not a factor\n") 
      else if (is.numeric(y) == FALSE)
          cat(deparse(substitute(y)), " is not numeric\n")
          else {
   boxplot(y ~ x, boxwex = box.cex, ylab = ylab, xlab = xlab, ylim = ylim, main = "", 
      outpch = NA, whisklty = 1, boxfill = box.col)
   points(jitter(rep(1:nlevels(x), table(x)[1:nlevels(x)]), .3), 
      jitter(unlist(split(y, x)), (max(y, na.rm = TRUE) -  min(y, na.rm = TRUE))/ 10),  
      pch = p.pch, cex = p.cex, col = p.col)
      }
}

# using default options
boxdata(qci$company, qci$accuracy, xlab = "Company", ylab = "Accuracy")

# changing some options for colour
boxdata(qci$gender, qci$jobsat, xlab = "Gender", ylab = "Job Satisfaction", 
   box.col = "grey90", p.col = "salmon")


######################################################
#   Hybrid histogram-density-boxplot (Figure 5.26)   #
######################################################

# A number of packages offer a hybrid histogram-boxplot option.
# I'll use the edaplot() from the StatDA package
# because it includes a scatterplot in the hybrid. 

library(StatDA)
 edaplot(qci$speed[!is.na(qci$speed)], H.freq = FALSE, 
    B.col = "skyblue", B.pch = 19, 
    S.pch = 19, S.cex = .5, S.col = "red", 
    P.main = "Hybrid histogram with density curve, boxplot and scatterplot", P.xlab = "Speed", P.ylab = "Density", 
    D.lwd = 2)
detach(package:StatDA)

# edaplot() has no option for changing the colour of the density line.
# But edaplot() can be modified.
# This is important advantage of using of R 
# and demonstrates another way in which R is free:
# the code that others have written is always available (using the get() function), 
# and can be modified or adapted to suit particular needs.
# Below is the code for the edaplot function,
# but with two modifications to allow colour for the density line to be spefified.
# The modified function is edaplot.m()

library(beanplot)
get(beanplot)
edaplot.m = function (data, scatter = TRUE, box = TRUE, P.plot = TRUE, D.plot = TRUE, 
    P.main = paste("Histogram of", deparse(substitute(data))), 
    P.sub = NULL, P.xlab = deparse(substitute(data)), P.ylab = default, 
    P.ann = par("ann"), P.axes = TRUE, P.frame.plot = P.axes, 
    P.log = FALSE, P.logfine = c(2, 5, 10), P.xlim = NULL, P.cex.lab = 1.4, 
    B.range = 1.5, B.notch = FALSE, B.outline = TRUE, B.border = par("fg"), 
    B.col = NULL, B.pch = par("pch"), B.cex = 1, B.bg = NA, H.breaks = "Sturges", 
    H.freq = TRUE, H.include.lowest = TRUE, H.right = TRUE, H.density = NULL, 
    H.angle = 45, H.col = NULL, H.border = NULL, H.labels = FALSE,
    S.pch = ".", S.col = par("col"), S.bg = NA, S.cex = 1, D.lwd = 1, 
    D.lty = 1, D.col = "black")               # D.col added with black as default
{
    xhi <- hist(data, plot = FALSE, breaks = H.breaks, include.lowest = H.include.lowest, 
        right = H.right)
    bxp <- boxplot(data, plot = FALSE, range = B.range, notch = B.notch, 
        outline = B.outline)
    dens <- density(data)
    if (P.plot) {
        x <- xhi$breaks
        if (H.freq) {
            y <- xhi$counts
            default = "Frequency"
            D.plot <- FALSE
        }
        else {
            y <- xhi$density
            default = "Relative frequency"
        }
        h <- -max(y, dens$y)/8
        if (scatter && box) {
            a <- 2
        }
        else if ((scatter & !box) | (!scatter && box)) {
            a <- 1
        }
        else {
            a <- 0
        }
        if (D.plot) {
            plot(x, c(y, a * h), type = "n", main = P.main, sub = P.sub, 
                xlab = P.xlab, ylab = P.ylab, ann = P.ann, frame.plot = P.frame.plot, 
                ylim = c(min(c(y, a * h)), max(y, dens$y)), xaxt = "n", 
                yaxt = "n", xlim = P.xlim, cex.lab = P.cex.lab)
            if (P.axes) {
                ay <- axTicks(2)
                axis(2, at = ay[ay >= 0], labels = ay[ay >= 0])
                if (P.log) 
                  axis(1, at = log10(alog <- sort(c((10^(-50:50)) %*% 
                    t(P.logfine)))), labels = alog)
                else axis(1)
            }
            lines(dens, lwd = D.lwd, lty = D.lty, col = D.col)         # col = D.col added
        }
        else {
            plot(x, c(y, a * h), type = "n", main = P.main, sub = P.sub, 
                xlab = P.xlab, ylab = P.ylab, ann = P.ann, axes = P.axes, 
                frame.plot = P.frame.plot, xlim = P.xlim, cex.lab = P.cex.lab)
        }
        hist(data, add = TRUE, axes = FALSE, breaks = H.breaks, 
            freq = H.freq, include.lowest = H.include.lowest, 
            right = H.right, density = H.density, angle = H.angle, 
            col = H.col, border = H.border, labels = H.labels)
        if (scatter && box) {
            rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 
                0)
            points(data, runif(length(data), 0.1, 0.9) * h, pch = S.pch, 
                col = S.col, bg = S.bg, cex = S.cex)
            rect(xhi$breaks[1], 2 * h, xhi$breaks[length(xhi$breaks)], 
                0)
            boxplot(data, add = TRUE, horizontal = TRUE, boxwex = -h * 
                1.2, at = h * 1.5, axes = FALSE, range = B.range, 
                notch = B.notch, outline = B.outline, border = B.border, 
                col = B.col, pch = B.pch, bg = B.bg, cex = B.cex, 
                xlim = P.xlim)
        }
        else if (scatter && !box) {
            rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 
                0)
            points(data, runif(length(data), 0.1, 0.9) * h, pch = S.pch, 
                col = S.col, bg = S.bg, cex = S.cex)
        }
        else if (!scatter && box) {
            rect(xhi$breaks[1], h, xhi$breaks[length(xhi$breaks)], 
                0)
            boxplot(data, add = TRUE, horizontal = TRUE, boxwex = -h * 
                1.2, at = h * 0.5, axes = FALSE, range = B.range, 
                notch = B.notch, outline = B.outline, border = B.border, 
                col = B.col, pch = B.pch, bg = B.bg, cex = B.cex, 
                xlim = P.xlim)
        }
    }
    l <- list(H = xhi, B = bxp)
    return(invisible(l))
}

# After running the code for the modified function, 
# the edaplot.m() function allows a coloured density line.

edaplot.m(qci$speed[!is.na(qci$speed)], H.freq = FALSE, H.col = rgb(1, 1, 0, .2), 
    B.col = "steelblue", B.pch = 19, 
    S.pch = 19, S.cex = .5, S.col = "hotpink", 
    P.main = "Hybrid histogram with density curve, boxplot and scatterplot", 
	P.xlab = "Speed", P.ylab = "Density", 
    D.lwd = 2, D.col = "orange")



###################################################
#   Violin plots (and bean plots) (Figure 5.27)   #
###################################################

library(caroline)   # for violin plots

layout(matrix(c(1,2), ncol = 2), respect = FALSE)

# violins() takes a list of vectors. split() creates a list.
# Here, split() creates list of five elements (one for each company),
# each element containing accuracy scores.

data = split(qci$accuracy, qci$company)
violins(data, connect = FALSE, ci.median_mu = FALSE,
   col = rep(rgb(1, 1, 0, .2), length(data)), pchMed = 20, colMed = "red", 
   rectCol = "steelblue")
title(main = expression(paste("Violin Plots: ", italic("accuracy"))), 
   xlab = "Company", ylab = "Accuracy")

data = split(qci$speed, qci$company)
violins(data, connect = FALSE, ci.median_mu = FALSE,
   col = rep(rgb(1, 1, 0, .2), length(data)), pchMed = 20, colMed = "red", 
   rectCol = "steelblue")
title(main = expression(paste("Violin Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")
layout(1)

# Using stats = TRUE, violins() also produces a table of descriptive statistics.

violins(data, connect = FALSE, ci.median_mu = FALSE, stats = TRUE,
   col = rep(rgb(1, 1, 0, .2), length(data)), pchMed = 20, colMed = "red", 
   rectCol = "steelblue")
title(main = expression(paste("Violin Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")

detach(package:caroline)

# The box plot (and the violin plot because it contains a boxplot)
# have the disadvantage that they might not be easily interpretable by non-mathematicians.
# The bean plot has been touted as an alternative. It gives:
# the density; the mean for each bean; the overall mean;
# and the data points. 
# The beans can be symmetric or asymmetric. 

library(beanplot)   # for bean plots
layout(matrix(c(1,2), ncol = 2), respect = FALSE)

beanplot(qci$accuracy ~ qci$company, las = 1, 
   beanlines = "median", overalline = "median")
title(main = expression(paste("Bean Plots: ", italic("accuracy"))), 
   xlab = "Company", ylab = "Accuracy")

beanplot(qci$speed ~ qci$company, las = 1, log = "",   # stops speed being plotted on a log scale.
   beanlines = "median", overalline = "median")
title(main = expression(paste("Bean Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")

layout(1)

# Each Bean plot can be split; for instance, each plot below is split by Gender
# i.e., asymmetric beans

layout(matrix(c(1,2), ncol = 2), respect = FALSE)

beanplot(qci$accuracy ~ qci$gender * qci$company, las = 1, side = "both", 
   border = NA, col = list("steelblue", "orange"),
   beanlines = "median", overalline = "median")
legend("topright", bty = "n", fill = c("steelblue", "orange"), legend = c("Male", "Female"))
title(main = expression(paste("Bean Plots: ", italic("accuracy"))), 
   xlab = "Company", ylab = "Accuracy")

beanplot(qci$speed ~ qci$gender * qci$company, las = 1, log = "", side = "both", 
   border = NA, col = list("steelblue", "orange"),
   beanlines = "median", overalline = "median")
legend("topright", bty = "n", fill = c("steelblue", "orange"), legend = c("Male", "Female"))
title(main = expression(paste("Bean Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")
   
layout(1)

detach(package:beanplot)

devAskNewPage(ask=FALSE)