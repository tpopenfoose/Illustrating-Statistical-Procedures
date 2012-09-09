# Procedure 5.6 Exploratory data analysis

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions 
# from the aplpack, StatDA, beanplot, ggplot2 and gridExtra packages.
# To install the packages, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages(c("aplpack", "StatDA", "beanplot", "ggplot", "gridExtra"), dependencies = TRUE)

# There are many graphics outputs to follow.
# Running all the code in one block will result 
# in the graphs flashing by too quickly.
# One way to fix this is to run the code in smaller 
# segments, or even one line at time.
# Alternatively, remove the # from the following line of code, and run it. 
# devAskNewPage(ask=TRUE)
# The command means that the user needs to supply a prompt before 
# moving on to the next graphic.
# At the first blank graphics window, right-click or press ENTER to obtain 
# the first output, then right-clisk or press ENTER to progress through the outputs.
# At the last line of code, there is a corresponnding devAskNewPage command 
# to turn off the user prompts. 

###########################################
#   Stem and Leaf display (Figure 5.22)   #
###########################################

stem(qci$accuracy, scale = .5)

# The stem.leaf() function in the aplpack draws the classic Tukey-style 
# stem-and-leaf display.

library(aplpack)
stem.leaf(qci$accuracy)

# The number of leaves per stem is determined by the software.
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

################# Extra for experts #########################################################

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

######################  END Extra for Experts  ##################################################


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


################# Extra for experts #########################################################

# edaplot() has no option for changing the colour of the density line.
# But edaplot() can be modified.
# This is important advantage of using of R 
# and demonstrates another way in which R is free:
# the code that others have written is always available (using the get() function), 
# and can be modified or adapted to suit particular needs.
# Below is the code for the edaplot() function,
# but with two modifications to allow colour for the density line to be specified.
# The modified function is edaplot.m()

library(StatDA)
get(edaplot)
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

detach(package:StatDA)

######################  END Extra for Experts  ##################################################


###################################################
#   Violin plots (and bean plots) (Figure 5.27)   #
###################################################

library(ggplot2)   # for violin plots
library(gridExtra)

p1 = ggplot(na.omit(qci), aes(company, accuracy)) +
   geom_violin(fill = "grey90") + 
   geom_boxplot(fill = "salmon", outlier.colour = NA, width = .1) +
   theme_bw() + 
   opts(panel.grid.major = theme_blank(),
        panel.grid.minor = theme_blank())

p2 = ggplot(na.omit(qci), aes(company, speed)) +
   geom_violin(fill = "grey90") + 
   geom_boxplot(fill = "salmon", outlier.colour = NA, width = .1) +
   theme_bw() + 
   opts(panel.grid.major = theme_blank(),
        panel.grid.minor = theme_blank())

grid.arrange(p1,p2, ncol=2)

title(main = expression(paste("Violin Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")

title(main = expression(paste("Violin Plots: ", italic("accuracy"))), 
   xlab = "Company", ylab = "Accuracy")


title(main = expression(paste("Violin Plots: ", italic("speed"))), 
   xlab = "Company", ylab = "Speed")



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

# devAskNewPage(ask=FALSE)