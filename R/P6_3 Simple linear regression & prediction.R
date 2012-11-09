# Procedure 6.3 Simple linear regression & prediction

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code used in the Procedure uses functions 
# from the ggplot2 package.
# To install the package, run the line of code below that 
# begins with "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages("ggplot2")


###################
#   Figure 6.8    #
###################

# The plot is drawn using the ggplot2 package.

library(ggplot2)
p <- ggplot(qci, aes(workcond, accuracy)) +          # Data frame and variables
   geom_point(position = position_jitter(width = .05, height = 0)) +   # plot points
   stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +          # plot regression line
   geom_vline(xintercept = 0, colour = "grey") +    # draw vertical line at x = 0
   expand_limits(x = 0) +                           # ensure the graph extends to 0
   scale_x_continuous(breaks = seq(0, 7, 1)) +      # modifies x-axis tick mark labels
   theme_bw() +                                     # removes the grey background
   labs(x = "WORKCOND", y = "ACCURACY") +     # Axis labels
   theme(panel.grid.major = element_blank(),        # removes the grid lines
         panel.grid.minor = element_blank())
p


#################################
#   Simple Linear Regression    #
#################################

# Summary of linear regression
m1 <- lm(accuracy ~ workcond, data = qci)
summary(m1)

# The regression parameters are given under "Estimates"
# part way down the output, 
# and the r-square is given by "Multiple R-squared"
# one line from the bottom of the table.

# To get the values on their own:
# Extract y-intercept, regression weight, and r-squared
coef(m1)[1]             # y-intercept
coef(m1)[2]             # regression weight
summary(m1)$r.squared   # r-squared 

# The coefficients do not agree with those in ISP.
# I'm not sure why.
# The coefficients calculated here agree with calculations using the formulas for OLS regression.


##################
#   Prediction   #
##################
# Predicted accuracy score for inspector 54
(PredictedAccuracy <- coef(m1)[1] + coef(m1)[2] * 3)
(ObservedAccuracy <- qci[qci$inspector == 54, ]$accuracy)
(Residual <- ObservedAccuracy - PredictedAccuracy)

# Alternatively, predicted values and residuals are available from m1
PA <- fitted.values(m1)              # All the predicted scores
PA54 <- PA[which(names(PA) == 54)]   # Predicted score for Inspector 54
PA54

Resid <- residuals(m1)                       # All the residuals
Resid54 <- Resid[which(names(Resid) == 54)]  # Residual for Inspector 54
Resid54


# How many Inspectors report a workcond score of 3
(who <- subset(qci, workcond == 3, select = "workcond"))  # Who are the inspectors?
dim(who)[1]                                              # How many?
                                     # but note, it does not agree with the number reported in ISP

# What were their residuals?
(Resid3 <- Resid[m1$model$workcond == 3])    # Residuals of Inspectors reporting workcond of 3

# Which of these Inspectors has the smallest absolute residual?
(MinResid <- min(abs(Resid3)))                          # The smallest of their residuals
(Insp <- names(which(round(abs(Resid3), 5) == round(MinResid, 5))))  # Which Inspector has this residual?
qci[qci$inspector == Insp, ]$accuracy                  # That Inspector's observed accuracy




